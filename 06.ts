//export const raw = "0\t2\t7\t0"
export const raw = "10	3	15	10	5	15	5	15	9	2	5	8	5	2	3	6"

const start = raw.split('\t').map(x => parseInt(x))

const biggestBank = (banks: number[]) => 
    banks.reduce((max, val, i) => val > banks[max] ? i : max, 0)

function redist(banks: number[]) {
    const result = banks.slice()
    let i = biggestBank(banks)
    let val = result[i]
    result[i++] = 0
    while(val) {
        result[i++ % banks.length]++
        val--
    }
    return result
}

const banksEquals = (banks: number[]) =>
    (b: number[]) => b.every((v,i) => v === banks[i])

const history: number[][] = []

const findInHistory = (banks: number[]) => 
    history.find(banksEquals(banks))

let step = 0
let dup: number[] | undefined
let next = start
do {
    history.push(next)
    next = redist(history[step++])
    dup = findInHistory(next)
} while (!dup)

console.log(step)

// part 2
step = 0
const cycle = banksEquals(dup)
do {
    next = redist(next)
    step++
} while (!cycle(next))
console.log(step)
