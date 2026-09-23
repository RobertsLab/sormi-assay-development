#!/usr/bin/env python3
"""
05.0 - Measure oyster surface area (mm^2) from 20260921 multi-stress plate photos.

Each photo shows one 12-well plate (3 rows x 4 cols, one oyster per well) above
a mm ruler. For each image we:
  1. estimate the pixel/mm scale from the periodicity of the ruler's mm ticks
     (FFT of the tick band intensity profile),
  2. fit the 3x4 well grid (known 26 mm pitch; search rotation/scale/offset)
     to a local-darkness map,
     (on darkness + brown tint)
  3. within each well disk, segment the oyster with a per-well Otsu threshold
     on darkness + saturation relative to that well's background, keeping the
     component that reaches the well centre (drops well-rim arcs),
  4. write per-well area (mm^2) and an overlay PNG for visual QC.

Outputs:
  Resazurin/outputs/05.0-measure-oyster-area-20260921/oyster_area.csv
  Resazurin/outputs/05.0-measure-oyster-area-20260921/overlays/plate-*.png
"""
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw
from scipy import ndimage as ndi

ROOT = Path(__file__).resolve().parents[1]  # Resazurin/
IMG_DIR = ROOT / "data" / "20260921-mgig-sormi-multi_stress" / "images"
OUT_DIR = ROOT / "outputs" / "05.0-measure-oyster-area-20260921"
OVL_DIR = OUT_DIR / "overlays"
OVL_DIR.mkdir(parents=True, exist_ok=True)

PLATES = list("CRSTUV")
DS = 2  # downsample factor for segmentation speed
PITCH_MM = 26.0     # 12-well plate well-to-well pitch (SBS format)
WELL_R_MM = 10.5    # search radius inside a well (well ID ~22 mm; excludes rim)
CORE_R_MM = 5.0     # a real oyster blob must reach within this of the centre
MIN_FEAT = 0.06     # floor on the per-well threshold
OTSU_FRAC = 0.6     # fraction of Otsu threshold (captures pale shell margins)
ROWS, COLS = "ABC", 4


def ruler_scale(gray):
    """Return (px_per_mm, ruler_top_row) in full-resolution pixels.

    Scans rows in the lower half of the image for a strong periodic signal at
    the mm-tick frequency (period ~12-40 px), then refines the period on the
    best band via FFT peak interpolation.
    """
    h, w = gray.shape
    best = (0, None, None)
    periods = (12, 40)
    for y in range(h // 2, h - 10, 6):
        band = gray[y:y + 12, :].mean(axis=0)
        band = band - ndi.uniform_filter1d(band, 101)
        spec = np.abs(np.fft.rfft(band * np.hanning(w)))
        freqs = np.fft.rfftfreq(w)
        m = (freqs > 1 / periods[1]) & (freqs < 1 / periods[0])
        k = np.argmax(spec[m])
        strength = spec[m][k] / (np.median(spec[m]) + 1e-9)
        if strength > best[0]:
            best = (strength, y, 1 / freqs[m][k])
    _, y_best, _ = best
    # refine on a thicker band with parabolic peak interpolation
    band = gray[y_best:y_best + 20, :].mean(axis=0)
    band = band - ndi.uniform_filter1d(band, 101)
    n = 1 << 16
    spec = np.abs(np.fft.rfft(band * np.hanning(w), n=n))
    freqs = np.fft.rfftfreq(n)
    m = (freqs > 1 / periods[1]) & (freqs < 1 / periods[0])
    idx = np.where(m)[0]
    k = idx[np.argmax(spec[idx])]
    a, b, c = spec[k - 1], spec[k], spec[k + 1]
    off = 0.5 * (a - c) / (a - 2 * b + c)
    period = 1 / ((k + off) / n)
    return period, y_best, best[0]


def evidence(rgb, px_per_mm):
    """Per-pixel 'oyster-ness' (downsampled): darker than local background."""
    small = rgb[::DS, ::DS].astype(float) / 255.0
    mx = ndi.uniform_filter(small.max(axis=2), 3)
    elem = int(30 * px_per_mm / DS) // 4
    bg = ndi.grey_closing(mx[::4, ::4], size=(elem, elem))
    bg = ndi.zoom(bg, (mx.shape[0] / bg.shape[0], mx.shape[1] / bg.shape[1]), order=1)
    return np.clip(bg - mx, 0, None), mx


def fit_grid(dark, px_per_mm, ruler_top):
    """Fit the 3x4 well grid (known pitch) to the darkness map.

    Searches rotation, pitch scale and origin, maximising darkness summed inside
    12 disks of radius ~8 mm (the oysters sit in the well centres).
    Returns a (3, 4, 2) array of well centres (y, x) in downsampled pixels.
    """
    f = 4  # extra coarsening for the search
    d = dark[::f, ::f].copy()
    d[int(ruler_top / DS / f):, :] = 0
    best = (-1, None)
    for scale in np.arange(1.02, 1.161, 0.02):  # plate top ~5-15% larger than ruler plane
        pitch = PITCH_MM * scale * px_per_mm / DS / f
        r = 6 * px_per_mm / DS / f  # fixed radius so larger pitch is not favoured
        yy, xx = np.mgrid[-int(r):int(r) + 1, -int(r):int(r) + 1]
        disk = (yy ** 2 + xx ** 2 <= r ** 2).astype(float)
        smap = ndi.convolve(d, disk, mode="constant")
        for theta in np.deg2rad(np.arange(-5, 5.1, 1.0)):
            c, s_ = np.cos(theta), np.sin(theta)
            offs = []
            for i in range(3):
                for j in range(4):
                    dx, dy = j * pitch, i * pitch
                    offs.append((dy * c + dx * s_, dx * c - dy * s_))
            offs = np.array(offs)
            oy, ox = np.round(offs).astype(int).T
            H, W = smap.shape
            y0max, x0max = H - oy.max() - 1, W - ox.max() - 1
            y0min, x0min = -oy.min(), -ox.min()
            if y0max <= y0min or x0max <= x0min:
                continue
            tot = np.zeros((y0max - y0min, x0max - x0min))
            for a, b in zip(oy, ox):
                tot += smap[y0min + a:y0max + a, x0min + b:x0max + b]
            k = np.unravel_index(np.argmax(tot), tot.shape)
            if tot[k] > best[0]:
                centres = (offs + np.array([k[0] + y0min, k[1] + x0min])) * f
                best = (tot[k], (centres.reshape(3, 4, 2), scale))
    return best[1]


def otsu(x):
    hist, edges = np.histogram(x, bins=128)
    mids = (edges[:-1] + edges[1:]) / 2
    w0 = np.cumsum(hist); w1 = w0[-1] - w0
    m0 = np.cumsum(hist * mids) / np.maximum(w0, 1)
    m1 = (np.sum(hist * mids) - np.cumsum(hist * mids)) / np.maximum(w1, 1)
    return mids[np.argmax(w0 * w1 * (m0 - m1) ** 2)]


def segment_well(val, sat, cy, cx, r_px, r_core):
    """Segment the oyster inside one well disk; returns mask in full DS frame."""
    H, W = val.shape
    y0, y1 = max(int(cy - r_px), 0), min(int(cy + r_px) + 1, H)
    x0, x1 = max(int(cx - r_px), 0), min(int(cx + r_px) + 1, W)
    yy, xx = np.mgrid[y0:y1, x0:x1]
    rr = np.hypot(yy - cy, xx - cx)
    disk = rr <= r_px
    v, s_ = val[y0:y1, x0:x1], sat[y0:y1, x0:x1]
    # well background = bright, unsaturated majority of the disk
    feat = (np.percentile(v[disk], 85) - v) + 1.5 * (s_ - np.percentile(s_[disk], 15))
    t = max(OTSU_FRAC * otsu(feat[disk]), MIN_FEAT)
    m = (feat > t) & disk
    m = ndi.binary_opening(m, structure=np.ones((5, 5)))
    m = ndi.binary_closing(m, structure=np.ones((9, 9)))
    m = ndi.binary_fill_holes(m)
    # disk opening (~1.5 mm) detaches thin well-rim arcs from the shell
    rad = max(int(1.5 * r_px / WELL_R_MM / 1.0), 3)
    dy, dx = np.mgrid[-rad:rad + 1, -rad:rad + 1]
    m = ndi.binary_opening(m, structure=(dy ** 2 + dx ** 2) <= rad ** 2)
    lab, n = ndi.label(m)
    if n == 0:
        return None
    # keep components that reach the well core (drops rim arcs)
    keep = [k for k in range(1, n + 1) if (rr[lab == k] <= r_core).any()]
    if not keep:
        return None
    sizes = [np.sum(lab == k) for k in keep]
    comp = lab == keep[int(np.argmax(sizes))]
    out = np.zeros_like(val, dtype=bool)
    out[y0:y1, x0:x1] = comp
    return out


rows_out = ["plate_ID,plate_well,area_px,area_mm2,px_per_mm,scale_strength,grid_scale,otsu_note"]
for p in PLATES:
    im = Image.open(IMG_DIR / f"plate-{p}.jpg").convert("RGB")
    rgb = np.asarray(im)
    gray = rgb.mean(axis=2)
    px_per_mm, ruler_top, strength = ruler_scale(gray)

    dark, val = evidence(rgb, px_per_mm)
    small = rgb[::DS, ::DS].astype(float) / 255.0
    mx, mn = small.max(axis=2), small.min(axis=2)
    # brown/olive tint (R,G above B); pink resazurin carry-over has high B so is not counted
    sat = ndi.uniform_filter(np.clip(small[..., :2].mean(axis=2) - small[..., 2], 0, None), 3)
    centres, gscale = fit_grid(dark, px_per_mm, ruler_top)
    # grid pitch is measured at the plate top, which sits closer to the camera
    # than the ruler; well radii follow the fitted pitch, areas use the ruler scale
    r_px = WELL_R_MM * gscale * px_per_mm / DS
    r_core = CORE_R_MM * gscale * px_per_mm / DS

    ovl = Image.fromarray(rgb[::DS, ::DS].copy())
    arr = np.asarray(ovl).copy()
    labels = []
    for i, row_letter in enumerate(ROWS):
        for j in range(COLS):
            cy, cx = centres[i, j]
            well = f"{row_letter}{j + 1:02d}"
            m = segment_well(val, sat, cy, cx, r_px, r_core)
            if m is None:
                area_px, area_mm2 = 0, float("nan")
            else:
                arr[m] = (0.5 * arr[m] + 0.5 * np.array([255, 0, 180])).astype(np.uint8)
                area_px = m.sum() * DS * DS
                area_mm2 = area_px / px_per_mm ** 2
            labels.append((cx, cy, r_px, f"{well} {area_mm2:.0f}"))
            rows_out.append(f"plate-{p},{well},{area_px:.0f},{area_mm2:.2f},"
                            f"{px_per_mm:.3f},{strength:.1f},{gscale:.2f},")
    ovl = Image.fromarray(arr)
    draw = ImageDraw.Draw(ovl)
    for cx, cy, r, txt in labels:
        draw.ellipse([cx - r, cy - r, cx + r, cy + r], outline=(0, 160, 255), width=2)
        draw.text((cx - 30, cy + r * 0.7), txt, fill=(0, 0, 0))
    draw.line([(0, ruler_top / DS), (ovl.width, ruler_top / DS)], fill=(0, 160, 255), width=3)
    draw.text((20, 20), f"plate-{p}  {px_per_mm:.2f} px/mm  grid x{gscale:.2f}", fill=(0, 0, 0))
    ovl.save(OVL_DIR / f"plate-{p}.png")
    print(f"plate-{p}: {px_per_mm:.2f} px/mm, grid scale {gscale:.2f}")

(OUT_DIR / "oyster_area.csv").write_text("\n".join(rows_out) + "\n")
