import Elm from "./Main.elm";

const node = document.getElementById("root");

node.innerHTML = "";

const app = Elm.Main.init({ node });
