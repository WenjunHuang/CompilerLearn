import { expect } from '@jest/globals';
import { char, concat } from '../NFA';

test("concat two simple characters", () => {
    const a = char('A');
    const b = char('B');
    expect(concat(a, b).test('AB')).toBe(true);
    expect(concat(b, a).test('BA')).toBe(true);
    expect(concat(a, b).test('BA')).toBe(false);
});

test("concat three simple characters", () => {
    const a = char('A');
    const b = char('B');
    const c = char('C');
    expect(concat(a, b, c).test('ABC')).toBe(true);
    expect(concat(a, b, c).test('BAC')).toBe(false);
});