#!/usr/bin/env python

import re
import sys

class TextStat(object):
    def __init__(self):
        self.string_to_count = {}

    def update(self, c):
        self.string_to_count[c] = self.string_to_count.get(c, 0) + 1

    def summary(self):
        count = sum(self.string_to_count.values())
        null_count = self.string_to_count.get('', 0)
        strs = [s for s in self.string_to_count.keys() if len(s) != 0]
        lengths = [len(s) for s in strs]
        lengths.sort() 
        least_len = lengths[0]
        most_len = lengths[-1]
        shortest_str = sorted([s for s in strs if len(s) == least_len])[0]
        longest_str = sorted([s for s in strs if len(s) == most_len])[0]
        num_shortest = self.string_to_count[shortest_str]
        num_longest = self.string_to_count[longest_str]
        avg_len = sum(self.string_to_count[s] * len(s) for s in strs) / float(count - null_count)

        yield ' count:                  %s' % count
        yield ' null count:             %s' % null_count
        yield ' count(shortest str):    %s %s' % (num_shortest, shortest_str)
        yield ' count(longest str):     %s %s' % (num_longest, longest_str)
        yield ' average length:         %.2f' % avg_len


class NumberStat(object):
    def __init__(self):
        self.null_count = 0
        self.count = 0
        self.total = 0.0
        self.least = float('inf')
        self.most = float('-inf')

    def update(self, c):
        self.count += 1
        if c == '':
            self.null_count += 1
        else:
            num = float(c)
            self.total += num
            self.least = min(self.least, num)
            self.most = max(self.most, num)
    
    def summary(self):
        avg = self.total / float(self.count - self.null_count)
        yield ' count:      %s' % self.count
        yield ' null_count: %s' % self.null_count
        yield ' min:        %.3f' % self.least
        yield ' max:        %.3f' % self.most
        yield ' total:      %.3f' % self.total
        yield ' avg:        %.3f' % avg


field_type_rx = re.compile(r'\(([^)]*)\)')
header_clean_rx = re.compile(r'"|\(.*?\)')

header_line = sys.stdin.readline()
header_cleaned = header_clean_rx.sub('', header_line)
field_types = field_type_rx.findall(header_line)
field_names = [s.strip() for s in header_cleaned.split(',')]

def make_stat_for_type(t):
    if t == 'number':
        return NumberStat()
    else:
        return TextStat()

stats = [make_stat_for_type(t) for t in field_types]

for line in sys.stdin:
    line = line.strip()
    row = line.split(',')
    for c, stat in zip(row, stats):
        stat.update(c) 

for name, s in zip(field_names, stats):
    print '%s:' % name
    print '\n'.join(s.summary())
    print

