import { read } from '../utils.js'

const res = read('1')
  .split('\n')
  .map(str => Number.parseInt(str, 10))
  .flatMap((a, _, arr) =>
    arr.flatMap(b =>
      arr.map(c => 
        [a, b, c]
      )
    )
  )
  .map(([a, b, c]) => {
    if (a + b + c === 2020) {
      return a * b * c
    }
    return null
  })
  .filter(v => v !== null)
  
console.log(res)
