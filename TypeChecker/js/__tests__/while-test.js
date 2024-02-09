const Type = require('../src/Type');
const { exec, test } = require('./test-util');

module.exports = eva => {
    test(eva,`(<= 1 10)`,Type.boolean);
    // If expression: both branches
    // should return the same type
    test(eva,
        `
        (var x 10)
        (while (!= x 0)
            (set x (- x 1))
        )
        x
        `, Type.number);
}