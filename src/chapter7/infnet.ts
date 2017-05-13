export type Arc =
    | {
        kind: 'indep'
        arcs: Arc[]
    }
    | {
        kind: 'arc'
        name: Node
        sufficiency: number
        necessity: number
    }
    | {
        kind: 'and'
        arcs: Arc[]
    }
function indep(...arcs: Arc[]): Arc {
    return { kind: 'indep', arcs }
}
function arc(node: Node, sufficiency: number, necessity: number): Arc {
    return { kind: 'arc', node, sufficiency, necessity }
}
function and(...arcs: Arc[]): Arc {
    return { kind: 'and', arcs }
}
export class Node {
    constructor(public name: string,
                public priorProb: number,
                public currentProb: number,
                public arc?: Arc) {
    }
}

// Primary Evidential Variables
const decor = new Node('decor', 0.5, 0.9)
const tableSetting = new Node('table-setting', 0.5, 0.8)
const surfaceCleanliness = new Node('surface-cleanliness', 0.8, 0.8)
const air = new Node('air', 0.6, 0.6)
const sounds = new Node('sounds', 0.5, 0.5)
const clientele = new Node('clientele', 0.5, 0.9)
const menu = new Node('menu', 0.5, 0.5)
const prices = new Node('prices', 0.5, 0.9)
const service = new Node('service', 0.3, 0.9)

// Lumped Evidential Variables
const popularity = new Node('popularity', 0.5, 0.6,
                            indep(
                                arc(sounds, 1.5, 1.0),
                                arc(clientele, 1.0, 0.24)))

const elegance = new Node('elegance', 0.5, 0.5,
                          indep(
                              arc(decor, 3.0, 0.5),
                              arc(tableSetting, 1.0, 0.74),
                              arc(sounds, 1.5, 0.74),
                              arc(clientele, 1.0, 0.5),
                              arc(menu, 1.24, 0.74),
                              arc(prices, 1.24, 0.74),
                              arc(service, 1.0, 0.5)))
const artistry = new Node('artistry', 0.5, 0.9,
                          indep(
                              arc(decor, 1.0, 0.5),
                              arc(tableSetting, 1.0, 0.5),
                              arc(menu, 1.5, 0.74),
                              arc(service, 1.0, 0.5)))
const cleanliness = new Node('cleanliness', 0.7, 0.7,
                             indep(
                                 arc(surfaceCleanliness, 1.5, 0.2),
                                 arc(air, 1.5, 0.5)))

// Predicted Component Variables
const taste = new Node('taste', 0.6, 0.6,
                       indep(
                           arc(popularity, 1.5, 0.7),
                           arc(elegance, 1.5, 0.8)))
const texture = new Node('texture', 0.6, 0.6,
                         indep(
                             arc(popularity, 1.5, 0.7),
                             arc(elegance, 1.0, 0.5)))
const appearance = new Node('appearance', 0.5, 0.5,
                            indep(
                                arc(artistry, 3.0, 0.4)))
const quantity = new Node('quantity', 0.5, 0.5,
                          indep(
                              arc(popularity, 1.5, 0.5)))
const correctness = new Node('correctness', 0.5, 0.5,
                             indep(
                                 arc(elegance, 1.0, 0.7)))
const nutrition = new Node('nutrition', 0.6, 0.6,
                           indep(
                               arc(popularity, 1.1, 0.7),
                               arc(elegance, 1.8, 0.8)))
const hygiene = new Node('hygiene', 0.8, 0.8,
                         indep(
                             arc(cleanliness, 1.0, 0.1)))

// Predicted Summary Variable
const overallFoodQuality = new Node('overall-food-quality', 0.5, 0.5,
                                    indep(
                                        and(
                                            arc(taste, 3.0, 0.3),
                                            arc(texture, 1.0, 0.5)),
                                        and(
                                            arc(appearance, 1.0, 0.3),
                                            arc(correctness, 1.3, 0.8)),
                                        arc(quantity, 1.2, 0.8),
                                        arc(nutrition, 1.0, 0.3),
                                        arc(hygiene, 1.5, 0.2)))

function updateProb(h: Node, arc: Arc): number {
}
