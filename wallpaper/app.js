// Picks today's species from the manifest, chooses the image for this screen's
// shape, and nudges the map and text a few pixels every hour so nothing sits
// on the same OLED pixels all day.
(function () {
  const data = window.RODENTS;
  const $ = (id) => document.getElementById(id);
  const fmt = (n) => (n == null ? "0" : Number(n).toLocaleString("en-GB"));
  const esc = (s) => String(s ?? "").replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));

  // Small cycles of offsets in pixels, one step per hour
  const mapShift = [[0, 0], [4, 2], [-3, 4], [2, -4], [-4, -2], [3, 3], [-2, -3], [4, -1]];
  const panelShift = [[0, 0], [-14, -24], [8, 18], [-6, 30], [12, -12], [-10, 8]];

  function dayIndex(now) {
    const [y, m, d] = data.start_date.split("-").map(Number);
    const start = new Date(y, m - 1, d);
    const today = new Date(now.getFullYear(), now.getMonth(), now.getDate());
    const days = Math.round((today - start) / 86400000);
    const n = data.species.length;
    return ((days % n) + n) % n;
  }

  // ?species=N in the address previews species N; Wallpaper Engine never sets it
  const preview = Number(new URLSearchParams(location.search).get("species"));
  const pick = (now) => (preview >= 1 && preview <= data.species.length ? preview - 1 : dayIndex(now));

  function render(now) {
    const sp = data.species[pick(now)];
    const screen = window.innerWidth / window.innerHeight > 2 ? "uw" : "hd";
    document.body.className = screen;
    document.documentElement.style.setProperty("--hue", sp.hue);
    document.documentElement.style.setProperty("--site", data.site_colour);

    $("map").src = sp.images[screen];
    $("eyebrow").textContent = `Rodent of the day · ${pick(now) + 1} of ${data.species.length}`;
    $("sci").textContent = sp.species;
    $("common").textContent = [sp.common_name, sp.family].filter(Boolean).join(" · ");
    $("sentence").textContent = sp.wiki_sentence || "";

    // Life history pairs
    $("traits").innerHTML = (sp.traits || []).map((t) => `<dt>${esc(t.label)}</dt><dd>${esc(t.value)}</dd>`).join("")
      || '<dt>No traits recorded</dt><dd></dd>';
    document.querySelector(".traits").style.display = (sp.traits || []).length ? "" : "none";

    // Seasonality ring: twelve segments from January at the top, clockwise,
    // opacity by share of the busiest month
    const months = sp.months || [];
    const peak = Math.max(1, ...months);
    const seg = (i, r0, r1) => {
      const a0 = (i / 12) * 2 * Math.PI - Math.PI / 2 + 0.03, a1 = ((i + 1) / 12) * 2 * Math.PI - Math.PI / 2 - 0.03;
      const p = (r, a) => `${(r * Math.cos(a)).toFixed(2)} ${(r * Math.sin(a)).toFixed(2)}`;
      return `M${p(r1, a0)} A${r1} ${r1} 0 0 1 ${p(r1, a1)} L${p(r0, a1)} A${r0} ${r0} 0 0 0 ${p(r0, a0)} Z`;
    };
    const initials = "JFMAMJJASOND";
    $("season").innerHTML = months.map((n, i) =>
      `<path d="${seg(i, 24, 38)}" fill="var(--hue)" fill-opacity="${(0.08 + 0.8 * n / peak).toFixed(2)}"><title>${initials[i]}: ${fmt(n)}</title></path>` +
      `<text x="${(45 * Math.cos(((i + 0.5) / 12) * 2 * Math.PI - Math.PI / 2)).toFixed(1)}" y="${(45 * Math.sin(((i + 0.5) / 12) * 2 * Math.PI - Math.PI / 2)).toFixed(1)}">${initials[i]}</text>`).join("");
    $("season").setAttribute("aria-label", "Records by month: " + months.map((n, i) => `${initials[i]} ${n}`).join(", "));
    document.querySelector(".season").style.display = months.some((n) => n > 0) ? "" : "none";

    // Variable importance: one series, bars scaled to the largest
    const imp = sp.importance || [];
    const top = Math.max(1, ...imp.map((d) => d.value));
    $("importance").innerHTML = imp.map((d) =>
      `<div class="bar-row"><span class="lab">${esc(d.label)}</span>` +
      `<span class="track"><span class="bar" style="display:block;width:${(100 * d.value / top).toFixed(1)}%"></span></span>` +
      `<span class="val">${Math.round(d.value)}%</span></div>`).join("");

    $("rangekey").style.display = sp.has_iucn_range ? "" : "none";

    const a = sp.arha || {};
    if (a.studies) {
      const found = (a.detections || []).slice(0, screen === "hd" ? 3 : 4).map((d) =>
        `<li>${esc(d.pathogen)} <span class="assay">· ${fmt(d.n_positive)} positive · ${esc(d.assays)}</span></li>`).join("");
      $("arha").innerHTML = `<h2>Project ArHa</h2>
        <div class="counts">${fmt(a.studies)} studies · ${fmt(a.individuals)} individuals · ${fmt(a.tested)} tests</div>
        ${found ? `<ul>${found}</ul>` : `<div class="none">No arenavirus or hantavirus detected</div>`}`;
    } else {
      $("arha").innerHTML = `<h2>Project ArHa</h2><div class="none">Not sampled for arenaviruses or hantaviruses</div>`;
    }

    const wiki = (sp.wiki_url || "").replace(/^https?:\/\//, "");
    const sil = sp.silhouette;
    $("silhouette").style.display = sil ? "" : "none";
    if (sil) $("silhouette").src = sil.image;
    const silCredit = sil ? `<br>Silhouette${sil.level === "species" ? "" : ` (${esc(sil.depicted)}, ${sil.level})`}: ${esc(sil.contributor)}, PhyloPic, ${esc(sil.licence)}` : "";

    $("source").innerHTML = `${fmt(sp.n_thinned)} GBIF records after thinning · BRT on bioclim + elevation<br>
      ${esc(sp.projection)} · borders GADM<br>GBIF.org, ${esc(sp.gbif_doi)}<br>${esc(wiki)}${silCredit}`;

    const h = now.getHours();
    const [mx, my] = mapShift[h % mapShift.length];
    const [px, py] = panelShift[Math.floor(h / 2) % panelShift.length];
    $("stage").style.transform = `translate(${mx}px, ${my}px)`;
    $("panel").style.transform = `translate(${px}px, ${py}px)`;

    // Records and globe only change with the species or the window size
    const motionKey = `${sp.key} ${screen} ${window.innerWidth}x${window.innerHeight}`;
    if (motionKey !== shownMotion) { shownMotion = motionKey; showRecords(sp, screen); showGlobe(sp, screen); }
  }

  // ---- Records: drawn by the page so they can be replayed by year -----------
  // The image's pixel grid is stretched to the window, so positions scale.
  let shownMotion = "";
  let pts = null, scale = { x: 1, y: 1 }, replaying = false;
  const REPLAY_MS = 50000, FLARE_MS = 1300, EVERY_MS = 10 * 60000;

  function sizeCanvas(c) {
    const dpr = window.devicePixelRatio || 1;
    c.width = Math.round(window.innerWidth * dpr);
    c.height = Math.round(window.innerHeight * dpr);
    const ctx = c.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    return ctx;
  }
  function dot(ctx, i) {
    ctx.fillRect(pts.x[i] * scale.x - 0.8, pts.y[i] * scale.y - 0.8, 1.6, 1.6);
  }
  function drawAllDots() {
    const ctx = sizeCanvas($("dots"));
    ctx.fillStyle = "rgba(255,255,255,0.45)";
    for (let i = 0; i < pts.x.length; i++) dot(ctx, i);
  }

  function showRecords(sp, screen) {
    const size = window.RODENT_SCREENS[screen];
    scale = { x: window.innerWidth / size.width, y: window.innerHeight / size.height };
    pts = null;
    sizeCanvas($("flares"));
    sizeCanvas($("dots"));
    const old = document.getElementById("points-script");
    if (old) old.remove();
    window.RODENT_POINTS = null;
    const s = document.createElement("script");
    s.id = "points-script";
    s.src = sp.points[screen];
    s.onload = () => { pts = window.RODENT_POINTS; if (pts) { drawAllDots(); setTimeout(replay, 4000); } };
    document.body.appendChild(s);
  }

  // Replay: records appear oldest first at a steady rate, each with a brief
  // flare in the species' hue, and a counter shows the year reached. The
  // canvas then holds still until the next replay, so the GPU idles.
  function replay() {
    if (!pts || replaying || document.hidden) return;
    replaying = true;
    const dots = sizeCanvas($("dots"));
    const flares = sizeCanvas($("flares"));
    dots.fillStyle = "rgba(255,255,255,0.45)";
    const hue = getComputedStyle(document.documentElement).getPropertyValue("--hue").trim();
    const n = pts.x.length;
    let undated = 0;
    while (undated < n && pts.year[undated] == null) dot(dots, undated++);
    let drawn = undated, active = [], t0 = null, lastFrame = 0;
    function frame(t) {
      if (t0 === null) t0 = t;
      if (t - lastFrame < 33) { requestAnimationFrame(frame); return; }   // about 30 frames a second
      lastFrame = t;
      const target = Math.min(n, undated + Math.floor((n - undated) * (t - t0) / REPLAY_MS));
      for (; drawn < target; drawn++) { dot(dots, drawn); active.push([drawn, t]); }
      flares.clearRect(0, 0, window.innerWidth, window.innerHeight);
      flares.fillStyle = hue;
      active = active.filter(([i, born]) => {
        const age = (t - born) / FLARE_MS;
        if (age >= 1) return false;
        flares.globalAlpha = 0.85 * (1 - age);
        flares.beginPath();
        flares.arc(pts.x[i] * scale.x, pts.y[i] * scale.y, 0.8 + 2.4 * (1 - age), 0, 2 * Math.PI);
        flares.fill();
        return true;
      });
      flares.globalAlpha = 1;
      if (drawn > undated) $("replay").textContent = `Records appearing by year · ${pts.year[drawn - 1]}`;
      if (drawn < n || active.length) { requestAnimationFrame(frame); return; }
      replaying = false;
      setTimeout(() => { $("replay").textContent = ""; }, 4000);
    }
    requestAnimationFrame(frame);
  }
  setInterval(replay, EVERY_MS);

  // ---- Globe: a sprite of frames round the world, cross-faded every few seconds
  let globeTimer = null;
  function showGlobe(sp, screen) {
    clearInterval(globeTimer);
    const g = sp.globe, el = $("globe");
    if (!g || !g.place || !g.place[screen]) { el.style.display = "none"; return; }
    const place = g.place[screen], size = window.RODENT_SCREENS[screen];
    const sx = window.innerWidth / size.width, sy = window.innerHeight / size.height;
    const side = place.size * Math.min(sx, sy);
    Object.assign(el.style, { display: "block", left: `${place.x * sx}px`, top: `${(place.y + place.size) * sy - side}px`,
                              width: `${side}px`, height: `${side}px` });
    const rows = Math.ceil(g.frames / g.cols);
    const frames = el.querySelectorAll(".frame");
    frames.forEach((f) => { f.style.backgroundImage = `url("${g.sprite}")`; f.style.backgroundSize = `${g.cols * 100}% ${rows * 100}%`; });
    const position = (i) => `${(i % g.cols) / (g.cols - 1) * 100}% ${Math.floor(i / g.cols) / Math.max(1, rows - 1) * 100}%`;
    let i = 0, front = 0;
    frames[0].style.backgroundPosition = position(0);
    frames[0].style.opacity = 1;
    frames[1].style.opacity = 0;
    globeTimer = setInterval(() => {
      if (document.hidden) return;
      i = (i + 1) % g.frames;
      const next = frames[1 - front];
      next.style.backgroundPosition = position(i);
      next.style.opacity = 1;
      frames[front].style.opacity = 0;
      front = 1 - front;
    }, 2500);
  }

  if (!data || !data.species || !data.species.length) {
    document.body.innerHTML = '<p style="color:#555;font:14px system-ui;padding:2em">manifest.js missing or empty</p>';
    return;
  }

  let last = "";
  function tick() {
    const now = new Date();
    const key = `${now.toDateString()} ${now.getHours()} ${window.innerWidth}x${window.innerHeight}`;
    if (key !== last) { last = key; render(now); }
  }
  tick();
  setInterval(tick, 60000);
  window.addEventListener("resize", tick);
})();
