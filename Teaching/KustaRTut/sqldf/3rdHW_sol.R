
# 1. ----------------------------------------------------------------------

x <- now(tzone = 'GMT')

# 2. ----------------------------------------------------------------------

y <- parse_date_time('200329', 'ymd', tz = 'GMT')
difftime(y, x, tz = 'GMT', units = 'weeks')
difftime(y, x, tz = 'GMT', units = 'days')
difftime(y, x, tz = 'GMT', units = 'hours')
difftime(y, x, tz = 'GMT', units = 'mins')
difftime(y, x, tz = 'GMT', units = 'secs')

# 3. ----------------------------------------------------------------------

x + dyears(1) + ddays(2) + dhours(12) + dminutes(3) + dseconds(20)