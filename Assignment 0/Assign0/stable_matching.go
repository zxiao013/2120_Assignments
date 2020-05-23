package main

import (
	"encoding/csv"
	"fmt"
	"io"
	"log"
	"os"
	"strconv"
	"sync"
)

var done chan bool

/**** PreferencesMatrix ****/
type PreferencesMatrix struct {
	subjects    []string
	preferences [][]string
	mux         sync.Mutex
}

func (p PreferencesMatrix) getPreferences(subject string) []string {
	var result []string
	for _, record := range p.preferences {
		if record[0] == subject {
			result = record
		}
	}
	return result
}
func (p PreferencesMatrix) itPreferThisOne(subject string, thisOne string, thatOne string) bool {
	itsPreferences := p.getPreferences(subject)
	return indexOf(thisOne, itsPreferences) < indexOf(thatOne, itsPreferences)
}
func (p PreferencesMatrix) thisRemoveThat(subject string, that string) {
	p.mux.Lock()
	var newPreferences []string
	for i, record := range p.preferences {
		if record[0] == subject {
			for _, object := range record {
				if object != that {
					newPreferences = append(newPreferences, object)
				}
			}
			p.preferences[i] = newPreferences
			break
		}
	}
	p.mux.Unlock()
}
func (p PreferencesMatrix) getMostPreferred(subject string) string {
	for _, record := range p.preferences {
		if record[0] == subject {
			return record[1]
		}
	}
	return ""
}
func NewPreferencesMatrix(records [][]string) PreferencesMatrix {
	var subjects []string
	for _, record := range records {
		subject := record[0]
		subjects = append(subjects, subject)
	}
	preferencesMatrix := PreferencesMatrix{
		subjects:    subjects,
		preferences: records}
	return preferencesMatrix
}

/**** Match ****/
type Match struct {
	employer string
	student  string
	isMatch  bool
}
type MatchMatrix struct {
	employers   []string
	students    []string
	matchMatrix []Match
	mux         sync.Mutex
	counter     int
}

func (m *MatchMatrix) printMatches() {
	var result string
	for _, match := range m.matchMatrix {
		if match.isMatch {
			result += match.employer + "," + match.student + "\n"
		}
	}
	problemSize := strconv.FormatInt(int64(len(m.employers)), 10)
	file, err := os.Create("matches_go_" + problemSize + "x" + problemSize + ".csv")
	if err != nil {
		return
	}
	_, err = file.WriteString(result)
	if err != nil {
		file.Close()
		return
	}
	err = file.Close()
	if err != nil {
		return
	}
}
func (m *MatchMatrix) studentHasMatch(student string) bool {
	var result bool
	for _, match := range m.matchMatrix {
		if match.student == student && match.isMatch {
			result = true
			break
		}
	}
	return result
}

func (m *MatchMatrix) employerHasMatch(employer string) bool {
	var result bool
	for _, match := range m.matchMatrix {
		if match.employer == employer && match.isMatch {
			result = true
			break
		}
	}
	return result
}

func (m *MatchMatrix) setMatch(employer string, student string) {
	m.mux.Lock()
	for i, match := range m.matchMatrix {
		if match.employer == employer && match.student == student {
			m.matchMatrix[i].isMatch = true
		}
		if match.employer == employer && match.student != student {
			m.matchMatrix[i].isMatch = false
		}
		if match.employer != employer && match.student == student {
			m.matchMatrix[i].isMatch = false
		}
	}
	m.counter++
	if m.counter == 3 {
		done <- true
	}
	m.mux.Unlock()
}
func (m *MatchMatrix) unsetMatch(employer string, student string) {
	m.mux.Lock()
	for i, match := range m.matchMatrix {
		if match.employer == employer && match.student == student {
			m.matchMatrix[i].isMatch = false
		}
	}
	m.counter--
	m.mux.Unlock()
}
func (m *MatchMatrix) getMatchedEmployerForStudent(student string) string {
	var result string
	for _, match := range m.matchMatrix {
		if match.student == student && match.isMatch {
			result = match.employer
		}
	}
	return result
}
func (m *MatchMatrix) evaluate(employer string, student string, coopEmployerPreferences *PreferencesMatrix, studentPreferences *PreferencesMatrix) {
	if !m.studentHasMatch(student) {
		m.setMatch(employer, student)
	} else {
		currentEmployer := m.getMatchedEmployerForStudent(student)
		if studentPreferences.itPreferThisOne(student, employer, currentEmployer) {
			m.unsetMatch(currentEmployer, student)
			m.setMatch(employer, student)
			go m.offer(currentEmployer, coopEmployerPreferences, studentPreferences)
		} else {
			coopEmployerPreferences.thisRemoveThat(employer, student)
			go m.offer(employer, coopEmployerPreferences, studentPreferences)
		}
	}
}

func (m *MatchMatrix) offer(employer string, coopEmployerPreferences *PreferencesMatrix, studentPreferences *PreferencesMatrix) {
	if !m.employerHasMatch(employer) {
		perferredStudent := coopEmployerPreferences.getMostPreferred(employer)
		m.evaluate(employer, perferredStudent, coopEmployerPreferences, studentPreferences)
	}
}

func NewMatchMatrix(employers []string, students []string) MatchMatrix {
	var matchMatrix []Match
	for _, employer := range employers {
		for _, student := range students {
			matchMatrix = append(matchMatrix, Match{
				employer: employer,
				student:  student,
				isMatch:  false})
		}
	}
	return MatchMatrix{
		employers:   employers,
		students:    students,
		matchMatrix: matchMatrix}
}

/**** Helper functions ****/
func readFileAndCreatePreferencesMatrix(csvFileName string) PreferencesMatrix {
	file, err := os.Open(csvFileName)
	if err != nil {
		log.Fatalln("error")
	}
	var records [][]string
	csvReader := csv.NewReader(file)
	for {
		record, err := csvReader.Read()
		if err == io.EOF {
			break
		}
		records = append(records, record)
	}
	preferencesMatrix := NewPreferencesMatrix(records)
	return preferencesMatrix
}
func indexOf(element string, slice []string) int {
	for i, v := range slice {
		if v == element {
			return i
		}
	}
	return -1
}

//
func main() {
	var coopEmployerPreferencesFileName string
	var studentPreferencesFileName string
	fmt.Println("Enter the file includes coop employer preferences:")
	fmt.Scanln(&coopEmployerPreferencesFileName)
	fmt.Println("Enter the file includes student preferences:")
	fmt.Scanln(&studentPreferencesFileName)
	coopEmployerPreferences := readFileAndCreatePreferencesMatrix(coopEmployerPreferencesFileName)
	studentPreferences := readFileAndCreatePreferencesMatrix(studentPreferencesFileName)
	employers := coopEmployerPreferences.subjects
	students := studentPreferences.subjects
	matchMatrix := NewMatchMatrix(employers, students)
	done = make(chan bool)
	for _, e := range employers {
		go matchMatrix.offer(e, &coopEmployerPreferences, &studentPreferences)
	}
	toPrint := <-done
	if toPrint {
		matchMatrix.printMatches()
	}
	close(done)
}
