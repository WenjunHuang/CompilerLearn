const Type = require('../src/Type');
const { test,exec } = require('./test-util');

module.exports = eva => {
    // Block: sequence of expressions.
    test(eva,
        ['begin',
            ['var', 'x', 10],
            ['var', 'y', 20],
            ['+', ['*', 'x', 10], 'y']],
        Type.number);
    test(eva,
        ['begin',
            ['var', 'x', 10],
            ['begin',
                ['var', 'x', '"hello"'],
                ['+', 'x', '"world"']],
            ['-', 'x', 5]
        ],
        Type.number);

    // Block: access parent scopes.
    test(eva,
        ['begin',
            ['var', 'x', 10],
            ['begin',
                ['var', 'y', 20],
                ['+', 'x', 'y']],
            ['-', 'x', 5]
        ],
        Type.number);

    // Block: variable update.
    test(eva,
        ['begin',
            ['var', 'x', 10],
            ['set', 'x', 20],
            ['begin',
                ['var', 'y', 20],
                ['set', 'x', ['+', 'x', 'y']]],
        ],
        Type.number);

    test(eva,
        `
        (begin 
            (var x 10)
            (var y 20)
            (+ (* x 10) y))`, Type.number);

    exec(eva,`(var x 10)`);
    test(eva,
          `(var y 20)
          (+ (* x 10) y)`, Type.number);
}