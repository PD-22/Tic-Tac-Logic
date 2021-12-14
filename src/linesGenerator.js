const fs = require('fs')
const PATH = './gen_res.txt'

// TODO: make solvable lines
/* 
  fill line completely
  remove random dots
*/

function generate(lineNum, lineLen) {
  // odd numbers not allowed
  if (
    lineNum < 6 || (lineNum % 2) != 0
    ||
    lineLen < 6 || (lineLen % 2) != 0
  ) {
    console.error('numbers must be (>=6) and even')
    return
  }

  const getRandChar = () => "xo."[Math.floor(Math.random() * 3)]
  const getArrayFillMap = (l, f) => Array(l).fill().map(f)
  const getRandLine = (l) => {
    let line;
    do {
      line = getArrayFillMap(l, getRandChar).join('');
    } while (!isValid(line));
    return line
  }
  for (let i = 0; i < lineNum; i++) {
    (async () => {
      console.clear()
      console.log(`${i + 1}/${lineNum}`)
      try {
        fs.appendFileSync(
          PATH,
          getRandLine(lineLen) + '\n',
          err => { if (err) console.error(err) }
        )
      } catch (error) {
        console.error(err)
      }
    })()
  }
}

function isValid(str) {
  function isBalanced() {
    let x = 0; let o = 0;
    Array.from(str).forEach(c => {
      if (c == 'x') x++
      else if (c == 'o') o++
    })
    return x == o
  }

  function hasTriples() {
    let pc, ppc;
    let chars = Array.from(str);
    for (let i = 0; i < str.length; i++) {
      const c = chars[i];
      if ((pc === ppc) && (ppc === c) && (c !== '.'))
        return true
      ppc = pc
      pc = c
    }
    return false
  }

  return isBalanced() && !hasTriples()
}

const [a, b] = process.argv.slice(2).map(parseFloat)

// empty first
fs.writeFile(PATH, '', err => { if (err) console.error(err) });
generate(a, b)
