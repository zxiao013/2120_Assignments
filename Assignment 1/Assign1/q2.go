package main

import (
	"fmt"
	"math"
)

type InputNeuron struct {
	c chan float64
}

func (inputNeuron InputNeuron) input(sin bool, k int, N int) {
	var X float64
	if sin {
		X = math.Sin(2 * math.Pi * float64(k-1) / float64(N))
	} else {
		X = math.Cos(2 * math.Pi * float64(k-1) / float64(N))
	}
	inputNeuron.c <- X
}

type HiddenNeuron struct {
	c chan float64
}

func (hiddenNeuron HiddenNeuron) calculate(a0 float64, a1 float64, a2 float64, X1c chan float64, X2c chan float64) {
	X1 := <-X1c
	X2 := <-X2c
	sum := a0 + a1*(X1) + a2*(X2)
	sum = 1 / (1 + math.Exp(-sum))
	hiddenNeuron.c <- sum
}

type OutputNeuron struct {
}

func (outputNeuron OutputNeuron) output(b0 float64, b1 float64, b2 float64, b3 float64, Z1c chan float64, Z2c chan float64, Z3c chan float64, k int) {
	Z1 := <-Z1c
	Z2 := <-Z2c
	Z3 := <-Z3c
	sum := b0 + b1*Z1 + b2*Z2 + b3*Z3
	sum = 1 / (1 + math.Exp(-sum))
	fmt.Println("The result of iteration", k, "is", sum)
}
func main() {
	var N int
	fmt.Println("How many iterations do you want?")
	fmt.Scanf("%d", &N)
	for i := 0; i < N; i++ {
		fmt.Println("This is iteration:", i)
		X1 := InputNeuron{make(chan float64)}
		X2 := InputNeuron{make(chan float64)}

		go X1.input(true, i, N)
		go X2.input(false, i, N)
		Z1 := HiddenNeuron{make(chan float64)}
		go Z1.calculate(0.1, 0.3, 0.4, X1.c, X2.c)

		go X1.input(true, i, N)
		go X2.input(false, i, N)
		Z2 := HiddenNeuron{make(chan float64)}
		go Z2.calculate(0.5, 0.8, 0.3, X1.c, X2.c)

		go X1.input(true, i, N)
		go X2.input(false, i, N)
		Z3 := HiddenNeuron{make(chan float64)}
		go Z3.calculate(0.7, 0.6, 0.6, X1.c, X2.c)

		T1 := OutputNeuron{}
		T1.output(0.5, 0.3, 0.7, 0.1, Z1.c, Z2.c, Z3.c, i)
	}
}
