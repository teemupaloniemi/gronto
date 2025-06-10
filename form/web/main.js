import { Fzf } from 'https://esm.sh/fzf';

class FZF_UI {
  constructor(data, inputID, outputID, selectedID) {
    // Data and search
    this.fzf = new Fzf(data);

    // HTML elements
    this.input  = document.getElementById(inputID);
    this.output = document.getElementById(outputID);
    this.selected = document.getElementById(selectedID);

    // Class buffers
    this.ss = new Set();
    this.rl = [];
  }

  renderResults() {
    this.rl = [];
    this.output.innerHTML = '';

    let i = 1;
    const answer = this.fzf.find(this.input.value);

    for (const result of answer) {

      const chars = result.item.split("");

      const nodes = chars.map((c, i) => {
        if (result.positions.has(i)) {
          return `<strong style="color: #f1563f">${c}</strong>`;
        } else {
          return c;
        }
      });

      const word = nodes.join("");

      const li = document.createElement('li');
      if (i < 9) li.innerHTML += `<kbd>[ctrl + ${i}]</kbd> ${word}`;
      else li.innerHTML += word;

      li.addEventListener('click', () => {
        if (!this.ss.has(result.item)) {
          this.ss.add(result.item);
          this.renderSelected();
        }
      });
      this.output.appendChild(li);
      this.rl.push(li);
      i += 1;
    }
  }

  renderSelected() {
    this.selected.innerHTML = '';
    for (const item of this.ss) {
      const li = document.createElement('li');
      li.addEventListener('click', (e) => {
          this.ss.delete(item);
          this.renderSelected();
      });
      li.innerHTML = item;
      this.selected.appendChild(li);
    }
  }

  keyHandler(e) {
    if (49 <= e.keyCode && e.keyCode < 57) {
      const idx = e.keyCode - 49;
      if (this.rl.length > idx) {
        this.rl[idx].click();
      }
    } else if (!e.ctrlKey) {
      this.renderResults();
    }
  }

  init() {
    this.input.addEventListener('keydown', (e) => { this.keyHandler(e); });
    //this.renderResults();
  }
}

function flatten(d) {
  let l = [];
  if (d == null) return l;
  for (const i of Object.keys(d)) {
    l.push(i);
    const nl = flatten(d[i]);
    if (nl != []) {
      l = l.concat(nl);
    }
  }
  return l;
}

async function get(url) {
  try {
    const response = await fetch(url);
    if (!response.ok) {
      throw new Error(`Failed to load data: ${response.status}`);
    }
    const json = await response.json();
    return json;
  } catch (error) {
    console.error(error.message);
    return [];
  }
}

function updateMailto() {
  const subject = encodeURIComponent("Jyu Course Questionnaire");
  const email = encodeURIComponent("tealjapa@student.jyu.fi");
  const body = encodeURIComponent(document.getElementById("json").textContent);
  const mailto = `mailto:${email}?subject=${subject}&body=${body}`;
  document.getElementById("email").href = mailto;
}

function updateRes() {
  const name = document.getElementById("name");
  const code = document.getElementById("code");
  const credits = document.getElementById("credits");
  const preqs = document.getElementById("sp").getElementsByTagName("li");
  const outcs = document.getElementById("so").getElementsByTagName("li");;
  const feelings = document.getElementById("feelings");
  const res = document.getElementById("json");
  const data = {};

  data["name"] = name.value;
  data["code"] = code.value;
  data["credits"] = credits.value;

  data["prerequisites"] = [];
  for (const p of preqs) {
    data["prerequisites"].push(p.innerText);
  }

  data["outcomes"] = [];
  for (const o of outcs) {
    data["outcomes"].push(o.innerText);
  }

  data["feelings"] = feelings.value;

  res.innerHTML = JSON.stringify(data, null, 2);
  updateMailto();
}

document.addEventListener('keydown', updateRes);
document.addEventListener('click', updateRes);

const data = await get("acm.json")

const fzfp = new FZF_UI(flatten(data), "ip", "op", "sp");
const fzfo = new FZF_UI(flatten(data), "io", "oo", "so");

fzfp.init();
fzfo.init();
updateRes();
