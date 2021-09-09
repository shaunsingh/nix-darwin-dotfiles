local present, icons = pcall(require, "nvim-web-devicons")
if not present then
   return
end

local colors = {
  bg = "NONE",
  white = "#ECEFF4",
  fg = "#E5E9F0",
  yellow = "#EBCB8B",
  cyan = "#A3BE8C",
  darkblue = "#81A1C1",
  green = "#8FBCBB",
  orange = "#D08770",
  purple = "#B48EAD",
  magenta = "#BF616A",
  gray = "#616E88",
  blue = "#5E81AC",
  red = "#BF616A"
}

icons.setup {
   override = {
      c = {
         icon = "",
         color = colors.blue,
         name = "c",
      },
      css = {
         icon = "",
         color = colors.blue,
         name = "css",
      },
      deb = {
         icon = "",
         color = colors.cyan,
         name = "deb",
      },
      Dockerfile = {
         icon = "",
         color = colors.cyan,
         name = "Dockerfile",
      },
      html = {
         icon = "",
         color = colors.baby_pink,
         name = "html",
      },
      jpeg = {
         icon = "",
         color = colors.dark_purple,
         name = "jpeg",
      },
      jpg = {
         icon = "",
         color = colors.dark_purple,
         name = "jpg",
      },
      js = {
         icon = "",
         color = colors.sun,
         name = "js",
      },
      kt = {
         icon = "󱈙",
         color = colors.orange,
         name = "kt",
      },
      lock = {
         icon = "",
         color = colors.red,
         name = "lock",
      },
      lua = {
         icon = "",
         color = colors.blue,
         name = "lua",
      },
      mp3 = {
         icon = "",
         color = colors.white,
         name = "mp3",
      },
      mp4 = {
         icon = "",
         color = colors.white,
         name = "mp4",
      },
      out = {
         icon = "",
         color = colors.white,
         name = "out",
      },
      png = {
         icon = "",
         color = colors.dark_purple,
         name = "png",
      },
      py = {
         icon = "",
         color = colors.cyan,
         name = "py",
      },
      toml = {
         icon = "",
         color = colors.blue,
         name = "toml",
      },
      ts = {
         icon = "ﯤ",
         color = colors.teal,
         name = "ts",
      },
      rb = {
         icon = "",
         color = colors.pink,
         name = "rb",
      },
      rpm = {
         icon = "",
         color = colors.orange,
         name = "rpm",
      },
      vue = {
         icon = "﵂",
         color = colors.vibrant_green,
         name = "vue",
      },
      xz = {
         icon = "",
         color = colors.sun,
         name = "xz",
      },
      zip = {
         icon = "",
         color = colors.sun,
         name = "zip",
      },
   },
}
