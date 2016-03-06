package main

import "bufio"
import "fmt"
import "log"
import "math"
import "os"
import "regexp"
import "sort"
import "strconv"
import "strings"

type Stat interface {
	update(cell string, lineNum int)
	summarize()
}

type TextStat struct {
	stringToCount map[string]int
}

func makeTextStat() *TextStat {
	return &TextStat{stringToCount: make(map[string]int)}
}

func (ts *TextStat) update(cell string, lineNum int) {
	_, ok := ts.stringToCount[cell]
	if ok {
		ts.stringToCount[cell]++
	} else {
		ts.stringToCount[cell] = 1
	}
}

func (ts *TextStat) summarize() {
	strs := make([]string, 0)
	lengths := make([]int, 0)
	count := 0
	nullCount := 0
	for s, c := range ts.stringToCount {
		count += c
		if s == "" {
			nullCount += c
		} else {
			strs = append(strs, s)
			lengths = append(lengths, len(s))
		}
	}
	sort.Ints(lengths)
	leastLen := lengths[0]
	mostLen := lengths[len(lengths)-1]

	shortestStrs := make([]string, 0)
	longestStrs := make([]string, 0)
	for _, s := range strs {
		if len(s) == leastLen {
			shortestStrs = append(shortestStrs, s)
		}
		if len(s) == mostLen {
			longestStrs = append(longestStrs, s)
		}
	}

	// Break ties alphabetically
	sort.Strings(shortestStrs)
	sort.Strings(longestStrs)
	shortestStr := shortestStrs[0]
	longestStr := longestStrs[0]

	numShortest := ts.stringToCount[shortestStr]
	numLongest := ts.stringToCount[longestStr]

	lenTotal := 0
	for s, c := range ts.stringToCount {
		lenTotal += c * len(s)
	}
	avgLen := float32(lenTotal) / float32(count-nullCount)

	fmt.Printf(" count:                  %d\n", count)
	fmt.Printf(" null count:             %d\n", nullCount)
	fmt.Printf(" count(shortest str):    %d %s\n", numShortest, shortestStr)
	fmt.Printf(" count(longest str):     %d %s\n", numLongest, longestStr)
	fmt.Printf(" average length:         %.2f\n", avgLen)
}

type NumberStat struct {
	nullCount int
	count     int
	total     float64
	least     float64
	most      float64
}

func makeNumberStat() *NumberStat {
	return &NumberStat{least: math.Inf(1), most: math.Inf(-1)}
}

func (self *NumberStat) update(cell string, lineNum int) {
	self.count++
	if cell == "" {
		self.nullCount++
	} else {
		num, err := strconv.ParseFloat(cell, 64)
		if err != nil {
			log.Fatalf("Failed to parse supposed float '%s' at line %d\n", cell, lineNum)
		}
		self.total += num
		self.least = math.Min(num, self.least)
		self.most = math.Max(num, self.most)
	}
}

func (self *NumberStat) summarize() {
	avg := self.total / float64(self.count-self.nullCount)
	fmt.Printf(" count:      %d\n", self.count)
	fmt.Printf(" null_count: %d\n", self.nullCount)
	fmt.Printf(" min:        %.3f\n", self.least)
	fmt.Printf(" max:        %.3f\n", self.most)
	fmt.Printf(" total:      %.3f\n", self.total)
	fmt.Printf(" avg:        %.3f\n", avg)
}

func main() {
	reader := bufio.NewReader(os.Stdin)

	// Read the header line, get the field names and types
	fieldTypeRx := regexp.MustCompile(`\(([^)]*)\)`)
	headerCleanRx := regexp.MustCompile(`"|\(.*?\)`)
	headerLine, err := reader.ReadString('\n')
	if err != nil {
		log.Fatalln("Failed to read header line.", err)
	}
	fieldTypes := fieldTypeRx.FindAllString(headerLine, -1)
	headerCleaned := headerCleanRx.ReplaceAllString(headerLine, "")
	fieldNames := strings.Split(headerCleaned, ",")
	for i, name := range fieldNames {
		fieldNames[i] = strings.TrimSpace(name)
	}

	// Init the stats
	stats := make([]Stat, len(fieldNames))
	for i, _ := range fieldNames {
		if fieldTypes[i] == "(number)" {
			stats[i] = makeNumberStat()
		} else {
			stats[i] = makeTextStat()
		}
	}

	// Read lines from stdin, updating the stats as we go
	for lineNum := 2; ; lineNum++ {
		line, err := reader.ReadString('\n')
		if err != nil {
			break
		}
		fields := strings.Split(line, ",")
		for i, field := range fields {
			field = strings.TrimSpace(field)
			stats[i].update(field, lineNum)
		}
	}

	// Print out the summary statistics
	for i, fieldName := range fieldNames {
		fmt.Printf("%s:\n", fieldName)
		stats[i].summarize()
		fmt.Println()
	}
}
