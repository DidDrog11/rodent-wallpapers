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

  function render(now) {
    const sp = data.species[dayIndex(now)];
    const screen = window.innerWidth / window.innerHeight > 2 ? "uw" : "hd";
    document.body.className = screen;
    document.documentElement.style.setProperty("--hue", sp.hue);
    document.documentElement.style.setProperty("--site", data.site_colour);

    $("map").src = sp.images[screen];
    $("eyebrow").textContent = `Rodent of the day · ${dayIndex(now) + 1} of ${data.species.length}`;
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
    $("source").innerHTML = `${fmt(sp.n_thinned)} GBIF records after thinning · BRT on bioclim + elevation<br>
      ${esc(sp.projection)} · borders GADM<br>GBIF.org, ${esc(sp.gbif_doi)}<br>${esc(wiki)}`;

    const h = now.getHours();
    const [mx, my] = mapShift[h % mapShift.length];
    const [px, py] = panelShift[Math.floor(h / 2) % panelShift.length];
    $("map").style.transform = `translate(${mx}px, ${my}px)`;
    $("panel").style.transform = `translate(${px}px, ${py}px)`;
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
