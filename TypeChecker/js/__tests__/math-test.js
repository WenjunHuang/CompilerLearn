
const Type = require('../src/Type');
const { test } = require('./test-util');

module.exports = eva => {
    // Math
    test(eva, ['+', 2, 3], Type.number);
    test(eva, ['-', 2, 3], Type.number);
    test(eva, ['*', 2, 3], Type.number);
    test(eva, ['/', 2, 3], Type.number);

    // String concat
    test(eva, ['+', '"hello,"', '"world"'], Type.string);
}