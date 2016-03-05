import datetime

sundays = 0
day = datetime.timedelta(days = 1)

start = datetime.date(1901, 1, 1)
end = datetime.date(2000, 12, 31)

while not (start == end):
    if (start.day == 1) and (start.weekday() == 6):
        sundays += 1
        
        print(start)
    
    start += day
    
print(sundays)