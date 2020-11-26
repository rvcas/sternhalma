/** @type {import("snowpack").SnowpackUserConfig } */
module.exports = {
  mount: {
    public: "/",
    src: "/_dist_",
  },
  plugins: [
    [
      "snowpack-plugin-elm",
      { verbose: false, debugger: "dev", optimize: "build" },
    ],
    [
      "@snowpack/plugin-build-script",
      { cmd: "postcss", input: [".css"], output: [".css"] },
    ],
  ],
  install: [],
  installOptions: {},
  devOptions: {},
  buildOptions: {},
  proxy: {},
  alias: {},
};
