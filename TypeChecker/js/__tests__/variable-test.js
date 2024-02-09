const Type = require('../src/Type');
const { test } = require('./test-util');

module.exports = eva => {
    // Variable declaration:
    test(eva, ['var', 'x', 10], Type.number);
    test(eva, ['var', ['y', 'number'], 10], Type.number);

    // Variable access:
    test(eva, 'x', Type.number);
    test(eva, 'y', Type.number);

    // Global variable:
    test(eva, 'VERSION', Type.string);
}