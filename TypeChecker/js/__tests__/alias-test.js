const { exec, test } = require('./test-util');
const Type = require('../src/Type');

module.exports = eva => {
    exec(eva,
        `
        (type int number)
        (type ID int)
        (type Index ID)
        `);

    test(eva,
        `
        (def square ((x int)) -> int (* x x))
        (square 2)
        `, Type.int);

    test(eva,
        `
        (def promote ((userId ID)) -> ID (+ 1 userId))
        (promote 1)
        `, Type.ID);

    test(eva,
        `
        (var (x Index) 1)
        x
        `, Type.Index);

    test(eva,
        `
        x
        `, Type.ID);

    test(eva,
        `
        x
        `, Type.int);
}