import { Fzf } from 'https://esm.sh/fzf';

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

const data = await get("acm.json")

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

class FZF_UI {
  constructor(d, iid, oid, sid) {
    // Data and search
    this.data = d;
    this.fzf = new Fzf(flatten(d));

    // HTML elements
    this.input  = document.getElementById(iid);
    this.output = document.getElementById(oid);
    this.selected = document.getElementById(sid);

    // Class buffers
    this.ss = new Set();
    this.rl = [];
  }

  search(obj, target, path = []) {
    for (const key in obj) {
      const newPath = [...path, key];
      if (key === target) return newPath;
      if (typeof obj[key] === 'object' && obj[key] !== null) {
        const result = this.search(obj[key], target, newPath);
        if (result) return result;
      }
    }
    return null;
  }

  renderTree(path) {
    let result = '';
    let depth  = 0;
    for (const item of path) {
      result += `${"    ".repeat(depth)}└── ${item}\n`;
      depth += 1;
    }
    return result;
  }

  renderResults(q) {
    this.rl = [];
    this.output.innerHTML = '';

    let i = 1;
    const answer = this.fzf.find(q);

    for (const result of answer) {

      const chars = result.item.split("");

      const nodes = chars.map((c, i) => {
        if (result.positions.has(i)) {
          return `<strong style="color: #f1563f">${c}</strong>`;
        } else {
          return c;
        }
      });


      const path = this.search(this.data, result.item);
      let word = "";
      if (path.length > 1) {
        path.pop();
        word = `<span style="color: grey;">${path.join("/")}/</span>${nodes.join("")}`;
      } else { 
        word = nodes.join("");
      }

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
    const c = e.keyCode;
    if (c == 13 && this.rl.length > 0) {
      this.rl[0].click();
      this.input.value = '';
    } else if ((/[a-z]/i).test(String.fromCharCode(c)) || e.keyCode == 32 || e.keyCode == 8) {
      if ((/[a-z]/i).test(String.fromCharCode(c)))
        this.renderResults(`${this.input.value}${e.key}`);
      else
        this.renderResults(this.input.value);
    } else if (49 <= c && c < 57) {
      const idx = c - 49;
      if (this.rl.length > idx) {
        this.rl[idx].click();
        this.input.value = '';
      }
    }
  }

  init() {
    this.input.addEventListener('keydown', (e) => { this.keyHandler(e); });
    this.renderResults("");
  }
}


function updateMailto() {
  const email_name = "tealjapa";
  const subject = encodeURIComponent("JYU Course Form");
  const email = encodeURIComponent(`${email_name}@student.jyu.fi`);
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

const fzfo = new FZF_UI(data, "io", "o", "so");
const fzfp = new FZF_UI(data, "ip", "o", "sp");

fzfp.init();
fzfo.init();

document.getElementById("name").focus();

updateRes();
