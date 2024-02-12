import { expect } from '@jest/globals';

import { char, or } from '../NFA';

test('simple characters', () => {
    const a = char('A');
    const b = char('B');
    const c = char('C');
    const d = char('D');

    expect(or(a, b, c, d).test('A')).toBe(true);
    expect(or(a, b, c, d).test('B')).toBe(true);
    expect(or(b, a, c, d).test('A')).toBe(true);
    expect(or(b, a, d, c).test('B')).toBe(true);
    expect(or(b, a, d, c).test('C')).toBe(true);
    expect(or(b, a, d, c).test('D')).toBe(true);
    expect(or(b, a, d, c).test('E')).toBe(false);
    expect(or(b, a, d, c).test('F')).toBe(false);
});