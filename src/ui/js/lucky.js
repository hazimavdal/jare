const alphabet =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789'

function randInt(max) {
    return Math.floor(Math.random() * Math.floor(max))
}

function randChar() {
    return alphabet[randInt(alphabet.length)]
}

function choose(arr, dist) {
    let rnd = Math.random()
    let acc = 0

    for (let i = 0; i < dist.length; i++) {
        acc += dist[i]

        if (acc >= rnd) return arr[i]
    }

    throw new Error('invalid distribution')
}

class RegExp {
    Epsilon = () => 'ε'
    Phi = () => '∅'
    Any = () => '.'
    Symbol = a => a
    Or = (a, b) => `(${a}|${b})`
    And = (a, b) => `(${a}&${b})`
    Concat = (a, b) => `(${a}${b})`
    Not = a => `~${a}*`
    Star = a => `${a}*`

    build(n) {
        if (n == 0) return this.Epsilon()
        if (n == 1) return this.Phi()
        if (n == 2) return this.Any()
        if (n == 3) return this.Symbol(randChar())
        if (n == 4) return this.Or(this.rand(), this.rand())
        if (n == 5) return this.And(this.rand(), this.rand())
        if (n == 6) return this.Concat(this.rand(), this.rand())
        if (n == 7) return this.Not(this.rand())
        if (n == 8) return this.Star(this.rand())

        throw new Error('invalid index')
    }

    rand() {
        let choices = [0, 1, 2, 3, 4, 5, 6, 7, 8]
        let dist = [0.15, 0.0, 0.15, 0.2, 0.1, 0.1, 0.1, 0.1, 0.1]

        let n = choose(choices, dist)

        return this.build(n)
    }
}