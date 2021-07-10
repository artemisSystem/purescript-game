const Main = require("../../../output/Example.AffGame.Main");

const colorGreen = "#6f7";
const colorBlue = "#67f";

function main () {

  console.log
    ( `%cStarting app. %cTime is ${new Date().toLocaleTimeString("nb-NO")}`
    , `color: ${colorGreen}`
    , `color: ${colorBlue}`
    );

  Main.main();
}

main();