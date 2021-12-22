if (process.env.NODE_ENV === "production") {
    const opt = require("./nazuki-opt.js");
    opt.main();
    module.exports = opt;
} else {
    var exports = window;
    exports.require = require("./nazuki-fastopt-entrypoint.js").require;
    window.global = window;

    const fastOpt = require("./nazuki-fastopt.js");
    fastOpt.main()
    module.exports = fastOpt;

    if (module.hot) {
        module.hot.accept();
    }
}
