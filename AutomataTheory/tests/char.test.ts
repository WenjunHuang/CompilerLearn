import { expect } from '@jest/globals';
import { char } from '../NFA';

test('char', () => {
    expect(char('A').test('A')).toBe(true);
    expect(char('B').test('A')).toBe(false);
});
