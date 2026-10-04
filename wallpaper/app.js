// Picks today's species from the manifest, chooses the image for this screen's
// shape, and nudges the map and text a few pixels every hour so nothing sits
// on the same OLED pixels all day.
(function () {
  const data = window.RODENTS;
  const $ = (id) => document.getElementById(id);
  const fmt = (n) => (n == null ? "0" : Number(n).toLocaleString("en-GB"));
  // Wallpaper Engine tells the page when it pauses it (a window covers the
  // desktop). document.hidden is no guide there: it may stay true while the
  // wallpaper is on screen, which would stop the replay and the globes.
  let paused = false;
  window.wallpaperPropertyListener = Object.assign(window.wallpaperPropertyListener || {}, { setPaused: (p) => { paused = !!p; } });
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
    document.documentElement.style.setProperty("--rec", sp.record_colour || "#ff5c8a");
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
    // This month: the segment outlined in neon red, drawn last so the outline sits on top
    const thisMonth = now.getMonth();
    $("season").insertAdjacentHTML("beforeend", `<path class="now" d="${seg(thisMonth, 24, 38)}"><title>This month</title></path>`);
    $("season").setAttribute("aria-label", "Records by month: " + months.map((n, i) => `${initials[i]} ${n}`).join(", ") + `; this month is ${initials[thisMonth]}`);
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
    if (motionKey !== shownMotion) { shownMotion = motionKey; showRecords(sp, screen); showGlobe(sp, screen); showBigGlobe(sp, screen); }
  }

  // ---- Records: drawn by the page so they can be replayed by year -----------
  // The image's pixel grid is stretched to the window, so positions scale.
  // Records wear their own neon (--rec), chosen to stand apart from the glow.
  let shownMotion = "";
  let pts = null, scale = { x: 1, y: 1 };
  let run = null;   // the replay in progress: { id, start, drawn, undated, active }
  let runs = 0;
  // ?replay=SECONDS in the address shortens the replay for previews; Wallpaper Engine never sets it
  const REPLAY_MS = (Number(new URLSearchParams(location.search).get("replay")) || 180) * 1000;
  const FLARE_MS = 2500, EVERY_MS = 15 * 60000, DOT_ALPHA = 0.55;

  function sizeCanvas(c) {
    const dpr = window.devicePixelRatio || 1;
    c.width = Math.round(window.innerWidth * dpr);
    c.height = Math.round(window.innerHeight * dpr);
    const ctx = c.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    return ctx;
  }
  const recColour = () => getComputedStyle(document.documentElement).getPropertyValue("--rec").trim();
  function dot(ctx, i) {
    ctx.fillRect(pts.x[i] * scale.x - 0.8, pts.y[i] * scale.y - 0.8, 1.6, 1.6);
  }
  function dotsContext(clear) {
    const ctx = clear ? sizeCanvas($("dots")) : $("dots").getContext("2d");
    ctx.fillStyle = recColour();
    ctx.globalAlpha = DOT_ALPHA;
    return ctx;
  }
  function drawAllDots() {
    const ctx = dotsContext(true);
    for (let i = 0; i < pts.x.length; i++) dot(ctx, i);
  }

  function showRecords(sp, screen) {
    const size = window.RODENT_SCREENS[screen];
    scale = { x: window.innerWidth / size.width, y: window.innerHeight / size.height };
    pts = null;
    run = null;
    $("replay").textContent = "";
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

  // Replay: records appear oldest first at a steady rate, each with a slow,
  // soft flare, and a counter shows the year reached. Progress follows the
  // clock, not the frame count, so if Wallpaper Engine pauses the page part
  // way through, the replay catches up when it resumes; a watchdog finishes
  // any replay that has stalled. Between replays nothing is redrawn.
  function replay() {
    if (!pts || run || paused) return;
    const dots = dotsContext(true);
    const n = pts.x.length;
    let undated = 0;
    while (undated < n && pts.year[undated] == null) dot(dots, undated++);
    run = { id: ++runs, start: performance.now(), drawn: undated, undated, active: [] };
    const id = run.id;
    setTimeout(() => frame(performance.now(), id), 50);
  }

  function finish(id) {
    if (!run || run.id !== id) return;
    const dots = dotsContext(false);
    for (; run.drawn < pts.x.length; run.drawn++) dot(dots, run.drawn);
    sizeCanvas($("flares"));
    run = null;
    setTimeout(() => { if (!run) $("replay").textContent = ""; }, 6000);
  }


  function frame(t, id) {
    if (!run || run.id !== id) return;   // a newer replay or a new species took over


    const n = pts.x.length, elapsed = t - run.start;
    if (elapsed > REPLAY_MS + FLARE_MS) { finish(id); return; }   // resumed after a long pause: just complete it
    const target = Math.min(n, run.undated + Math.floor((n - run.undated) * elapsed / REPLAY_MS));
    const dots = dotsContext(false);
    const catchingUp = target - run.drawn > 200;   // after a short pause: no burst of flares
    for (; run.drawn < target; run.drawn++) { dot(dots, run.drawn); if (!catchingUp) run.active.push([run.drawn, t]); }
    const flares = $("flares").getContext("2d");
    flares.clearRect(0, 0, window.innerWidth, window.innerHeight);
    flares.fillStyle = recColour();
    run.active = run.active.filter(([i, born]) => {
      const age = (t - born) / FLARE_MS;
      if (age >= 1) return false;
      flares.globalAlpha = 0.6 * (1 - age) * (1 - age);
      flares.beginPath();
      flares.arc(pts.x[i] * scale.x, pts.y[i] * scale.y, 0.8 + 1.6 * (1 - age), 0, 2 * Math.PI);
      flares.fill();
      return true;
    });
    flares.globalAlpha = 1;
    if (run.drawn > run.undated) $("replay").textContent = `Records appearing by year · ${pts.year[run.drawn - 1]}`;
    if (run.drawn < n || run.active.length) { setTimeout(() => frame(performance.now(), id), 50); return; }   // about 20 steps a second
    finish(id);
  }

  // Watchdog: a replay that has run well past its length has stalled (the page
  // was hidden mid-way and frames stopped), so complete it.
  setInterval(() => { if (run && performance.now() - run.start > REPLAY_MS + FLARE_MS + 30000) finish(run.id); }, 30000);
  setInterval(replay, EVERY_MS);

  // ---- Globe: a sprite of frames round the world, cross-faded every few seconds
  let globeTimer = null;
  function showGlobe(sp, screen) {
    clearInterval(globeTimer);
    const g = sp.globe, el = $("globe");
    if (!g || !g.place || !g.place[screen] || !(g.frames || g.sprite)) { el.style.display = "none"; return; }
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
      if (paused) return;
      i = (i + 1) % g.frames;
      const next = frames[1 - front];
      next.style.backgroundPosition = position(i);
      next.style.opacity = 1;
      frames[front].style.opacity = 0;
      front = 1 - front;
    }, 2500);
  }

  // ---- Big globe: one image per view, the next preloaded, cross-faded --------
  // One turn in globe frames x 5 seconds (four minutes for 48 views).
  let bigTimer = null;
  function showBigGlobe(sp, screen) {
    clearInterval(bigTimer);
    const g = sp.big_globe, el = $("bigglobe");
    if (!g || !g.place || !g.place[screen] || !g.frames || !g.frames.length) { el.style.display = "none"; return; }
    const place = g.place[screen], size = window.RODENT_SCREENS[screen];
    const sx = window.innerWidth / size.width, sy = window.innerHeight / size.height;
    const side = place.size * Math.min(sx, sy);
    Object.assign(el.style, { display: "block", left: `${(place.x + place.size / 2) * sx - side / 2}px`, top: `${place.y * sy}px`,
                              width: `${side}px`, height: `${side}px` });
    const views = el.querySelectorAll(".view");
    let i = g.start || 0, front = 0;
    views[0].src = g.frames[i];
    views[0].style.opacity = 1;
    views[1].style.opacity = 0;
    let preload = new Image();
    preload.src = g.frames[(i + 1) % g.frames.length];
    bigTimer = setInterval(() => {
      if (paused) return;
      i = (i + 1) % g.frames.length;
      const next = views[1 - front];
      next.onload = () => { next.style.opacity = 1; views[front].style.opacity = 0; front = 1 - front; next.onload = null; };
      next.src = g.frames[i];
      preload = new Image();
      preload.src = g.frames[(i + 1) % g.frames.length];
    }, 5000);
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
