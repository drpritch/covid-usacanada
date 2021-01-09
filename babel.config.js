module.exports = {
    plugins: [
//        "@babel/plugin-proposal-class-properties",
        "@babel/plugin-transform-runtime" // To enable async methods
    ],
    presets: [
        "@babel/typescript",
        "@babel/preset-react",
        [
            "@babel/env", { }
        ]
    ]
};
