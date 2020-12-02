import fs from 'fs'

export const read = (number) => fs.readFileSync(`${number}.in`, 'utf-8')