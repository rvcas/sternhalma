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
  ],
  install: [],
  installOptions: {},
  devOptions: {},
  buildOptions: {},
  proxy: {},
  alias: {},
};
