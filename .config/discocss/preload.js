module.exports = () => {
  const fs = require("fs");
  const confDir = "/home/shauryasingh/.config/discocss";
  const cssFile = "/home/shauryasingh/.config/discocss/custom.css";

  function reload(style) {
    style.innerHTML = fs.readFileSync(cssFile);
  }

  function inject({ document, window }) {
    window.addEventListener("load", () => {
      const style = document.createElement("style");
      reload(style);
      document.head.appendChild(style);

      fs.watch(confDir, {}, () => reload(style));
    });
  }

  inject(require("electron").webFrame.context);
};

module.exports.mw = (mainWindow) => {
  mainWindow.setBackgroundColor("#00000000");
};

module.exports.mo = (options) => {
  options.transparent = true;
  if (process.platform === "linux") {
    options.frame = true;
  }
};
