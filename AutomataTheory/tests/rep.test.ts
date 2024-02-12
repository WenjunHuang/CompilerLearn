import {expect} from '@jest/globals';
import { char, rep } from '../NFA';

test("rep",()=>{
    const re = rep(char('a'));
    expect(re.test('')).toBe(true);
    expect(re.test('a')).toBe(true);
    expect(re.test('aa')).toBe(true);
    expect(re.test('b')).toBe(false);
});