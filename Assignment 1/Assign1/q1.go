package main

import (
	"fmt"
	"time"
)

type Play struct {
	name      string
	purchased []Ticket
	showStart time.Time
	showEnd   time.Time
}

type Comedy struct {
	laughs float32
	deaths int32
	play   Play
}

func (comedy *Comedy) getName() string {
	return comedy.play.name
}

func (comedy *Comedy) getShowStart() time.Time {
	return comedy.play.showStart
}

func (comedy *Comedy) getShowEnd() time.Time {
	return comedy.play.showEnd
}

func (comedy *Comedy) addPurchase(ticket *Ticket) bool {
	var result bool
	if len(comedy.play.purchased) == cap(comedy.play.purchased) {
		result = false
	} else {
		comedy.play.purchased = append(comedy.play.purchased, *ticket)
		result = true
	}
	return result
}

func (comedy *Comedy) isNotPurchased(ticket *Ticket) bool {
	sold := false
	for i := 0; i < len(comedy.play.purchased); i++ {
		ticketSeat := ticket.s
		purchasedSeat := (comedy.play.purchased[i : i+1])[0].s
		if ticketSeat.number == purchasedSeat.number && ticketSeat.row == purchasedSeat.row {
			sold = true
			break
		}
	}
	return !sold
}

type Tragedy struct {
	laughs float32
	deaths int32
	play   Play
}

func (tragedy *Tragedy) getName() string {
	return tragedy.play.name
}

func (tragedy *Tragedy) getShowStart() time.Time {
	return tragedy.play.showStart
}

func (tragedy *Tragedy) getShowEnd() time.Time {
	return tragedy.play.showEnd
}

func (tragedy *Tragedy) addPurchase(ticket *Ticket) bool {
	var result bool
	if len(tragedy.play.purchased) == cap(tragedy.play.purchased) {
		result = false
	} else {
		tragedy.play.purchased = append(tragedy.play.purchased, *ticket)
		result = true
	}
	return result
}

func (tragedy *Tragedy) isNotPurchased(ticket *Ticket) bool {
	sold := false
	for i := 0; i < len(tragedy.play.purchased); i++ {
		ticketSeat := ticket.s
		purchasedSeat := (tragedy.play.purchased[i : i+1])[0].s
		if ticketSeat.number == purchasedSeat.number && ticketSeat.row == purchasedSeat.row {
			sold = true
			break
		}
	}
	return !sold
}

type Show interface {
	getName() string
	getShowStart() time.Time
	getShowEnd() time.Time
	addPurchase(*Ticket) bool
	isNotPurchased(*Ticket) bool
}

type Seat struct {
	number int32
	row    int32
	cat    *Category
}

type Category struct {
	name string
	base float32
}

type Ticket struct {
	customer string
	s        *Seat
	show     *Show
}

type Theatre struct {
	seats []Seat
	shows []Show
}

// NewSeat return a new Seat
func NewSeat(seatNumber int32, rowNumber int32, category *Category) Seat {
	return Seat{number: seatNumber, row: rowNumber, cat: category}
}

// NewTicket return a new Ticket
func NewTicket(customerName string, seat *Seat, show *Show) Ticket {
	return Ticket{customer: customerName, s: seat, show: show}
}

// NewTheatre return a new Theatre
func NewTheatre(seats []Seat, shows []Show) Theatre {
	return Theatre{seats: seats, shows: shows}
}

func main() {
	seats := make([]Seat, 25)
	for i := 1; i < 2; i++ {
		for j := 1; j < 6; j++ {
			category := Category{name: "Prime", base: 35.0}
			seats = append(seats, NewSeat(int32(j), int32(i), &category))
		}
	}
	for i := 2; i < 5; i++ {
		for j := 1; j < 6; j++ {
			category := Category{name: "Standard", base: 25.0}
			seats = append(seats, Seat{number: int32(j), row: int32(i), cat: &category})
		}
	}
	for i := 5; i < 6; i++ {
		for j := 1; j < 6; j++ {
			category := Category{name: "Special", base: 15.0}
			seats = append(seats, Seat{number: int32(j), row: int32(i), cat: &category})
		}
	}

	tartuffe := Play{name: "Tartuffe",
		purchased: make([]Ticket, 0, 25),
		showStart: time.Date(2020, time.March, 3, 16, 0, 0, 0, time.UTC),
		showEnd:   time.Date(2020, time.March, 3, 17, 20, 0, 0, time.UTC)}
	macbeth := Play{name: "Macbeth",
		purchased: make([]Ticket, 0, 25),
		showStart: time.Date(2020, 4, 16, 9, 30, 0, 0, time.UTC),
		showEnd:   time.Date(2020, 4, 16, 12, 30, 0, 0, time.UTC)}
	comedy := Comedy{laughs: 0.2, deaths: 0, play: tartuffe}
	tragedy := Tragedy{laughs: 0.0, deaths: 12, play: macbeth}
	shows := []Show{&comedy, &tragedy}
	theatre := NewTheatre(seats, shows)

	for {
		var intendedPlayName string
		fmt.Println("Please enter the play name:")
		fmt.Scanln(&intendedPlayName)

		fmt.Println("Please enter the row number:")
		var seatRow int32
		fmt.Scanf("%d", &seatRow)

		fmt.Println("Please enter the seat number:")
		var seatNumber int32
		fmt.Scanf("%d", &seatNumber)

		var intendedCat Category
		switch seatRow {
		case 1:
			intendedCat = Category{name: "Prime", base: 35.0}
		case 5:
			intendedCat = Category{name: "Special", base: 15.0}
		default:
			intendedCat = Category{name: "Standard", base: 25.0}
		}
		intendedSeat := Seat{number: int32(seatNumber), row: int32(seatRow), cat: &intendedCat}

		foundShow := false
		for showIndex, show := range theatre.shows {
			if show.getName() == intendedPlayName {
				foundShow = true
				intendedTicket := Ticket{s: &intendedSeat, show: &show}
				if theatre.shows[showIndex].isNotPurchased(&intendedTicket) {
					theatre.shows[showIndex].addPurchase(&intendedTicket)
					fmt.Printf("Your seat is row %d number %v", intendedSeat.row, intendedSeat.number)
					fmt.Printf(" for show %v starts from %v and ends at %v\n", show.getName(), show.getShowStart().Format("2 Jan 2006 15:04AM"), show.getShowEnd().Format("2 Jan 2006 15:04AM"))
					break
				} else {
					fmt.Println("Sorry, the seat is taken. We're trying to find an alternative seat...")
					found := false
					switch intendedSeat.row {
					case 1:
						// first search the same category
						for i := 1; i < 5; i++ {
							if int32(i) != intendedSeat.number {
								altSeat := Seat{number: int32(i), row: int32(1), cat: &intendedCat}
								altTicket := Ticket{s: &altSeat, show: &show}
								if theatre.shows[showIndex].isNotPurchased(&altTicket) {
									theatre.shows[showIndex].addPurchase(&altTicket)
									fmt.Printf("Good news! We allocate you a seat at row %v number %v", 1, i)
									fmt.Printf(" for show %v starts from %v and ends at %v\n", show.getName(), show.getShowStart().Format("2 Jan 2006 15:04AM"), show.getShowEnd().Format("2 Jan 2006 15:04AM"))
									found = true
									break
								}
							}
						}
						if !found {
							// then search standard seats
							for i := 2; i < 5; i++ {
								for j := 1; j < 6; j++ {
									if int32(i) != intendedSeat.row || int32(j) != intendedSeat.number {
										altSeat := Seat{number: int32(j), row: int32(i), cat: &Category{name: "Standard", base: 25.0}}
										altTicket := Ticket{s: &altSeat, show: &show}
										if theatre.shows[showIndex].isNotPurchased(&altTicket) {
											theatre.shows[showIndex].addPurchase(&altTicket)
											fmt.Printf("Good news! We allocate you a seat at row %v number %v", i, j)
											fmt.Printf(" for show %v starts from %v and ends at %v\n", show.getName(), show.getShowStart().Format("2 Jan 2006 15:04AM"), show.getShowEnd().Format("2 Jan 2006 15:04AM"))
											found = true
											break
										}
									}
								}
								if found {
									break
								} else {
									continue
								}
							}
						}
						if !found {
							// then search special seats
							for i := 1; i < 5; i++ {
								if int32(i) != intendedSeat.number {
									altSeat := Seat{number: int32(i), row: int32(5), cat: &Category{name: "Special", base: 15.0}}
									altTicket := Ticket{s: &altSeat, show: &show}
									if theatre.shows[showIndex].isNotPurchased(&altTicket) {
										theatre.shows[showIndex].addPurchase(&altTicket)
										fmt.Printf("Good news! We allocate you a seat at row %v number %v", 5, i)
										fmt.Printf(" for show %v starts from %v and ends at %v\n", show.getName(), show.getShowStart().Format("2 Jan 2006 15:04AM"), show.getShowEnd().Format("2 Jan 2006 15:04AM"))
										found = true
										break
									}
								}
							}
						}
					case 5:
						// search special seats only
						for i := 1; i < 5; i++ {
							if int32(i) != intendedSeat.number {
								altSeat := Seat{number: int32(i), row: int32(5), cat: &Category{name: "Special", base: 15.0}}
								altTicket := Ticket{s: &altSeat, show: &show}
								if theatre.shows[showIndex].isNotPurchased(&altTicket) {
									theatre.shows[showIndex].addPurchase(&altTicket)
									fmt.Printf("Good news! We allocate you a seat at row %v number %v", 5, i)
									fmt.Printf(" for show %v starts from %v and ends at %v\n", show.getName(), show.getShowStart().Format("2 Jan 2006 15:04AM"), show.getShowEnd().Format("2 Jan 2006 15:04AM"))
									found = true
									break
								}
							}
						}
					default:
						// search the standard seats first
						for i := 2; i < 5; i++ {
							for j := 1; j < 6; j++ {
								if int32(i) != intendedSeat.row || int32(j) != intendedSeat.number {
									altSeat := Seat{number: int32(j), row: int32(i), cat: &Category{name: "Standard", base: 25.0}}
									altTicket := Ticket{s: &altSeat, show: &show}
									if theatre.shows[showIndex].isNotPurchased(&altTicket) {
										theatre.shows[showIndex].addPurchase(&altTicket)
										fmt.Printf("Good news! We allocate you a seat at row %v number %v", i, j)
										fmt.Printf(" for show %v starts from %v and ends at %v\n", show.getName(), show.getShowStart().Format("2 Jan 2006 15:04AM"), show.getShowEnd().Format("2 Jan 2006 15:04AM"))
										found = true
										break
									}
								}
							}
							if found {
								break
							} else {
								continue
							}
						}
						if !found {
							// then search special seats
							for i := 1; i < 5; i++ {
								if int32(i) != intendedSeat.number {
									altSeat := Seat{number: int32(i), row: int32(5), cat: &Category{name: "Special", base: 15.0}}
									altTicket := Ticket{s: &altSeat, show: &show}
									if theatre.shows[showIndex].isNotPurchased(&altTicket) {
										theatre.shows[showIndex].addPurchase(&altTicket)
										fmt.Printf("Good news! We allocate you a seat at row %v number %v", 5, i)
										fmt.Printf(" for show %v starts from %v and ends at %v\n", show.getName(), show.getShowStart().Format("2 Jan 2006 15:04AM"), show.getShowEnd().Format("2 Jan 2006 15:04AM"))
										found = true
										break
									}
								}
							}
						}
					}
					if !found {
						fmt.Println("Sorry, we cannot find an alternative seat for you")
					}
				}
			}
		}
		if !foundShow {
			fmt.Println("Sorry, we don't have the show:", intendedPlayName)
		}
	}
}
