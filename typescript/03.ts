assert(1, 0)
assert(9, 2)
assert(10, 3)
assert(11, 2)
assert(12, 3)
assert(23, 2)
assert(26, 5)
assert(50, 7)
assert(1024, 31)
console.log(f(265149))

function assert(x: number, fx: number) {
    if (f(x) !== fx) {
        console.log(`failed of f(${x})`)
    } else {
        console.log(`f(${x}) is good`)
    }
}

function f(n: number) {
    if (n === 1) return 0;
    const [r, d] = ring(n)
    const hr = Math.floor(r / 2)
    const nn = n - (r-2) * (r-2)
    const ofs = Math.abs((nn % (r - 1)) - hr)
    return ofs + d
}

function ring(x: number) {
    let r = 3, d = 1
    while (x > (r * r)) {
        r += 2
        d += 1
    }
    return [r, d]
}

/// part 2

enum Dir { left = 0, right, up, down }
const vec = [[-1,0], [1,0], [0,-1], [0,1]]
const turn = [Dir.down, Dir.up, Dir.left, Dir.right]

function init() {
    let x = 0, y = 0
    let d = Dir.down

    const grid = new Map<string, number>()
    grid.set('0,0', 1)    

    function get(_x:number, _y:number) {
        const index = `${_x},${_y}`
        const value = grid.get(index)
        return value === undefined ? 0 : value
    }

    function isEmpty(_x:number, _y:number) {
        const index = `${_x},${_y}`
        return !grid.has(index)
    }

    function fill() {
        const index = `${x},${y}`
        const value = get(x-1, y-1) + get(x, y-1) + get (x+1, y-1) +
            get(x-1, y) + get(x+1, y) +
            get(x-1, y+1) + get(x, y+1) + get(x+1, y+1)
        grid.set(index, value)
        return value
    }

    function step() {
        const [cdx, cdy] = vec[turn[d]]
        if (isEmpty(x + cdx, y + cdy)) {
            d = turn[d]
        }
        const [dx, dy] = vec[d]
        x += dx
        y += dy
        return fill()
    }

    return step
}

const stepper = init()
let v: number
do {
    v = stepper()
    console.log(v)
} while (v <= 265149)
