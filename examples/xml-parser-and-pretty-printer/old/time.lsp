(xml-tree
   (xml-declaration (version "1.0" encoding "ISO-8859-1"))
   (declaration "DOCTYPE" "edoc PUBLIC \"-//Elucidator2 edoc//EN\" \"\"")
   (comment "initial comment ")
   (tree
      (tag start "edoc")
      (tree
         (tag start "enode" role "essay")
         (tree
            (tag start "head")
            (comment
               "The topic must characterize and introduce the thematic contents of ")
            (comment
               "the node, not merely categorize (label) it. The topic should aim ")
            (comment
               "to convey positions and results. Topics are more likely to be ")
            (comment
               "representative and topically faithful if they are (1) constructed as ")
            (comment
               "sentence fragments and (2) rewritten after composition of the node. ")
            (tree (tag start "topic") "Time Conversion ")
            (comment
               "The abstract is supposed to be a thematic window into the ")
            (comment
               "contents of the node. The abstract must boil down the node body ")
            (comment
               "to typicalle 3-5 lines of text. It should expose rationales, results ")
            (comment "and main characteristics of content of the node. ")
            (tree
               (tag start "abstract")
               "This program shows how to convert time from milliseconds elapsed since January 1, 1970, 00:00:00 GMT to a representation with year, month, day, hour, minutes and seconds."
               (tag start-end "br")
               (tag start-end "br")
               "It does so via a class that stores and convert the given time. The class has an interface that allow one to retrieve the needed information about the time."
               (tag start-end "br")
               (tag start-end "br")
               "A test example is provided to illustrate the use of the class."
               (tag start-end "br")
               (tag start-end "br")
               "This documentation is to be viewed as an small example of "
               (tree (tag start "em") "Elucidative Programming")
               "with Java and was part of the week-assignment for the authors DAT5 project exam."
               (tag start-end "br")
               (tag start-end "br"))
            (comment
               "The status of the node, being new, inprogress or finished. ")
            (tree (tag start "status") (tag start-end "new"))
            (comment "A list of representative keywords for the node. ")
            (tree
               (tag start "keywords")
               (tree (tag start "kw") "Time")
               (tree (tag start "kw") "Conversion")
               (tree (tag start "kw") "Elucidative programming"))
            (comment
               "The name of the author that has last change the node (Will be ")
            (comment "filled in automatically). ")
            (tree
               (tag start "author")
               "Max Rydahl Andersen and Claus Nyhus Christensen")
            (tree (tag start "created") "2000/09/12 21:43:32")
            (tree (tag start "last-updated") "$Date: 2000/09/04 11:55:46 $"))
         (tree
            (tag start "section")
            (tree (tag start "title") "Introduction")
            "This example is based on the "
            (tree
               (tag
                  start
                  "xlink"
                  role
                  "mentions"
                  href
                  "http://www.cs.auc.dk/~normark/elucidative-programming/time-conversion/time/time.html")
               "Scheme Time Example")
            "made by Kurt Noermark."
            (tag start-end "br")
            (tag start-end "br")
            "The goal is to create a Java application that can convert time from one representation to another. To do this we create a class named "
            (tree
               (tag start "slink" role "mentions" href "ugeopgave/Date")
               "Date")
            ", that can be used to store a specifc instance of time and provides an interface to set and get information about the time."
            (tag start-end "br")
            (tag start-end "br")
            "There exists a range of different time representations and many rules for calculation and exceptions to these rules. We are not going to discuss all the different methods of time conversions but focus on one of the simpler solutions: "
            (tree (tag start "em") "Universal Time")
            "(UT). UT is based on the elapsed time since January 1, 1970, 00:00:00 GMT. "
            (tag start-end "br")
            (tag start-end "br")
            "Javas own "
            (tree
               (tag start "slink" role "mentions" href "java/util/Date")
               "java.util.Date")
            "class uses milliseconds to store the elapsed time, in this example we are going to use seconds instead."
            (tag start-end "br")
            (tag start-end "br")
            "The following sections will first describe the structure of the class and secondly the methods used for doing the actual time conversion."
            (tag start-end "br")
            (tag start-end "br"))
         (tree
            (tag start "section" sbase "ugeopgave/Date")
            (tree (tag start "title") "Class structure")
            "The "
            (tree (tag start "slink" role "mentions" href "") "Date")
            "class stores the UT seconds in the "
            (tree (tag start "slink" role "describes" href "@ut") "ut")
            "field. This field can be set either through the class single parameter "
            (tree
               (tag start "slink" role "describes" href "@Date(long)")
               "constructor")
            "or via the "
            (tree
               (tag start "slink" role "describes" href "@setUT(long)")
               "setUT(long)")
            "method."
            (tag start-end "br")
            (tag start-end "br")
            "Whenever the "
            (tree (tag start "slink" role "mentions" href "@ut") "ut")
            "field is set, each part of the time is calculated. The method used for the calculation is described "
            (tree
               (tag
                  start
                  "dlink"
                  role
                  "mentions"
                  href
                  "/Time.edoc@conversionmethod")
               "later")
            ". Each part is stored in a slot and this is described in the following "
            (tree
               (tag start "dlink" role "mentions" href "/Time.edoc@slots")
               "section")
            ". "
            (tree
               (tag start "section" label "slots")
               (tree (tag start "title") "Slots")
               "We use slots to store each interesting item of a "
               (tree (tag start "em") "time")
               ". A slot is just a plain "
               (tree (tag start "em") "long")
               "and all slots are stored in the "
               (tree (tag start "slink" role "describes" href "@slots") "slots")
               "field."
               (tag start-end "br")
               (tag start-end "br")
               "The "
               (tree (tag start "code") "slots")
               "array contain currently five items. These are listed here with their corresponding value intervals. "
               (tree (tag start "slink" role "describes" href "@YEAR") "YEAR")
               "[1970..292278994]), "
               (tree (tag start "slink" role "describes" href "@MONTH") "MONTH")
               "[1..12], "
               (tree (tag start "slink" role "describes" href "@DAY") "DAY")
               "[1..31], "
               (tree (tag start "slink" role "describes" href "@HOUR") "HOUR")
               "[0..23], "
               (tree
                  (tag start "slink" role "describes" href "@MINUTE")
                  "MINUTE")
               "[0..59] and "
               (tree
                  (tag start "slink" role "describes" href "@SECONDS")
                  "SECONDS")
               "[0.59]."
               (tag start-end "br")
               (tag start-end "br")
               "The slots array is used to have each item pre-calculated. This removes the need for recalculation when each item of the time is accessed via the "
               (tree
                  (tag start "slink" role "describes" href "@e:slotgetset")
                  "get-methods")
               ". In a larger industrial application it probably will be more important to be able to write the human readable format for the time instead of the elapsed seconds since 1970. The \"slot-solution\" would be the best solution for this as the readable format is straightforward to extract. A more complete example of such an implementation is in the "
               (tree
                  (tag
                     start
                     "xlink"
                     href
                     " http://www.javasoft.com/j2se/1.3/docs/api/java/util/Date.html")
                  "standard Java API")
               (tag start-end "br")
               (tag start-end "br")
               "The disadvantage is the need for extra storage, but this is not an issue for this example. If only the elapsed seconds is needed in an application a solution is to just store the UT in a simple long and use the date class when it is needed to present the date in readable form."
               (tag start-end "br")
               (tag start-end "br"))
            (tree
               (tag start "section" label "conversionmethod")
               (tree (tag start "title") "Conversion method")
               "The conversion from UT(elapsed seconds) to a representation with year, month, day, hour, minute and seconds is handled by "
               (tree
                  (tag start "slink" role "describes" href "@calcTime()")
                  "calcTime()")
               "."
               (tag start-end "br")
               (tag start-end "br")
               "To convert from elapsed seconds to each part of the time can be done either through a formula or via succesive counting. A formula is most efficient but can be hard to comprehend, successive counting is just the opposite - not so efficient, but easier to comprehend."
               (tag start-end "br")
               (tag start-end "br")
               "We are going to use both methods, calculation for the simplest issues and successive counting for the rest."
               (tag start-end "br")
               (tag start-end "br")
               "The general strategy is to start with the total elapsed seconds and then subtract the seconds for each item in the time. We start out by calculating the "
               (tree
                  (tag start "slink" role "mentions" href "@calcTime()@e:a")
                  "year")
               "and the remainding seconds. From the seconds and year we calculate the exact "
               (tree
                  (tag start "slink" role "mentions" href "@calcTime()@e:b")
                  "day and month")
               "and from the last remaining seconds "
               (tree
                  (tag start "slink" role "mentions" href "@calcTime()@e:c")
                  "hour, minute and seconds")
               "is calculated."
               (tag start-end "br")
               (tag start-end "br")
               "After each calculation the relevant item is stored in their respective location, in the "
               (tree (tag start "slink" role "mentions" href "@slots") "slots")
               "field, which is described "
               (tree
                  (tag start "dlink" role "mentions" href "/Time.edoc@slots")
                  "else where")
               ". "
               (tag start-end "br")
               (tag start-end "br")
               "The internal details of the different calculations is described in their own section, "
               (tree
                  (tag start "dlink" role "mentions" href "/Time.edoc@year")
                  "year")
               ", "
               (tree
                  (tag start "dlink" role "mentions" href "/Time.edoc@daymonth")
                  "day/month")
               ", "
               (tree
                  (tag
                     start
                     "dlink"
                     role
                     "mentions"
                     href
                     "/Time.edoc@hourminsec")
                  "hour/minute/seconds")
               ". ")
            (tree
               (tag start "section" label "year")
               (tree (tag start "title") "Year")
               "To calculate which year a given UT time occur in the "
               (tree
                  (tag start "slink" role "describes" href "@calcYear(long)")
                  "calcYear(long)")
               "method uses successive counting."
               (tag start-end "br")
               (tag start-end "br")
               "It starts out with the "
               (tree
                  (tag start "slink" role "describes" href "@BASE_YEAR")
                  "base year")
               "and "
               (tree
                  (tag start "slink" role "mentions" href "@calcYear(long)@e:b")
                  "succesivly adds")
               "the number of seconds for the year in the loop until it ends up with "
               (tree
                  (tag start "slink" role "mentions" href "@calcYear(long)@e:a")
                  "less seconds")
               "than the next year contains."
               (tag start-end "br")
               (tag start-end "br")
               "The length of a given year is returned by the method "
               (tree
                  (tag start "slink" role "describes" href "@yearLength(long)")
                  "yearLength(long)")
               ", which check if the year is a "
               (tree
                  (tag start "dlink" role "mentions" href "/Time.edoc@leapyear")
                  "leap year")
               "and returns the correct amount of seconds. ")
            (tree
               (tag start "section" label "leapyear")
               (tree (tag start "title") "Finding leap year")
               "To handle leap years, we have the method "
               (tree
                  (tag start "slink" role "describes" href "@isLeapYear(long)")
                  "isLeapYear(long)")
               ". "
               (tag start-end "br")
               (tag start-end "br")
               "The method returns true if "
               (tree (tag start "code") "year")
               "is a leap year and false otherwise. "
               (tag start-end "br")
               (tag start-end "br")
               "It uses the following rules:"
               (tag start-end "br")
               "A year is only a leap year if it is divisible with 4 and not divisible by 100. An exception to this rule is that if the year is divisible by 400 it "
               (tree (tag start "em") "is")
               "a leap year. "
               (tag start-end "br")
               (tag start-end "br")))
         (tree
            (tag start "section" label "daymonth" sbase "ugeopgave/Date")
            (tree (tag start "title") "Month and day")
            "To find the month and day we have the method "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "describes"
                  href
                  "@calcDayAndMonth(long,long)")
               "calcDayAndMonth")
            ". This method receives the number of seconds elapsed since 1. January of the relevant year. Hence the number of the actual day is relatively easy to calculate by "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "describes"
                  href
                  "@calcDayAndMonth(long,long)@e:div")
               "division")
            "of a normal day length."
            (tag start-end "br")
            "The remaining seconds are saved and is later returned by this method."
            (tag start-end "br")
            (tag start-end "br")
            "The exact month is found by using a pre-calculated array that contains "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "describes"
                  href
                  "@calcDayAndMonth(long,long)@numOfDays")
               "number of days")
            "passed since the first in each of the twelve months. There are two pre-calculated arrays, one for "
            (tree
               (tag start "slink" role "describes" href "@MONTH_LENGTHS")
               "regular months")
            "and another for "
            (tree
               (tag start "slink" role "describes" href "@LEAP_MONTH_LENGTHS")
               "leap-year months")
            "."
            (tag start-end "br")
            (tag start-end "br")
            "By "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "describes"
                  href
                  "@calcDayAndMonth(long,long)@e:search")
               "searching through")
            "this array a month is found. The actual day in the month can be calculated by "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "describes"
                  href
                  "@calcDayAndMonth(long,long)@e:subtract")
               "subtracting")
            "the days stored in the found months entry in the array."
            (tag start-end "br")
            (tag start-end "br")
            "Note, we add 1 to the actual day as the calculation is based on the first day being 0(zero) and we want to deliver the result in the range from 1 to 31. ")
         (tree
            (tag start "section" label "hourminsec" sbase "ugeopgave/Date")
            (tree (tag start "title") "Hour, minute and second")
            "The "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "describes"
                  href
                  "@calcHourMinuteSeconds(long)")
               "calcHourMinuteSeconds(long)")
            "method receives the number of seconds elapsed in the day. The last parts of the time is easily calculated via division and modulo."
            (tag start-end "br")
            (tag start-end "br")
            "First we divide the seconds with the length of an hour to get the "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "mentions"
                  href
                  "@calcHourMinuteSeconds(long)@e:a")
               "correct hour")
            ". The "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "mentions"
                  href
                  "@calcHourMinuteSeconds(long)@e:b")
               "remainder")
            "of this division is found by modulo and this is used to find the "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "mentions"
                  href
                  "@calcHourMinuteSeconds(long)@e:c")
               "minute-number")
            "by dividing with the length of a minute. The "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "mentions"
                  href
                  "@calcHourMinuteSeconds(long)@e:d")
               "remainder")
            "of this division is finally stored as the "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "mentions"
                  href
                  "@calcHourMinuteSeconds(long)@e:e")
               "second-number")
            ". ")
         (tree
            (tag start "section" sbxase "ugeopgave/Date")
            (tree (tag start "title") "Miscellaneous")
            (comment
               "The topic must characterize and introduce the thematic contents of ")
            "This section contains some miscellaneous issues concerning the class."
            (tag start-end "br")
            (tag start-end "br")
            (tree (tag start "b") "Further work")
            (tag start-end "br")
            "The class can be extended with much more functionality, e.g. the reverse conversion would be highly relevant. If the class should be used to keep track of time it would be beneficial to look at issues such as timezones, leap-seconds, Gregorian vs. Julian calendar, etc."
            (tag start-end "br")
            (tag start-end "br")
            (tree (tag start "b") "Test")
            (tag start-end "br")
            "There also exist a class that "
            (tree
               (tag start "slink" role "describes" href "ugeopgave/DateTest")
               "tests")
            "the conversion and compares it with the "
            (tree
               (tag start "slink" role "mentions" href "java/util/Date")
               "standard Date class")
            "in Java. This test uses the "
            (tree
               (tag
                  start
                  "slink"
                  role
                  "mentions"
                  href
                  "ugeopgave/Date@toString()")
               "toString()")
            "to produce a more human readable format."
            (tag start-end "br")
            (tag start-end "br")
            (tree (tag start "b") "JavaDoc")
            (tag start-end "br")
            "To get a better overview of the class the automatically generated "
            (tree
               (tag
                  start
                  "xlink"
                  href
                  "http://dopu.cs.auc.dk/data/maxtime/ugeopgave/apidoc/")
               "JavaDoc")
            "is a good place to start. ")))
   (comment "Final comment "))

