(((kind "manual-section") (section-title "About the format of calendar input.") (section-body "
The calendar input must be present in a variable named calendar-events.
calendar-events must be defined externally to the calendar tool.
Calendar-events is a list of appointments.
A single appointment is, in turn, a list. Thus calendar-events is a list of lists.<p>

The detailed format of an appointment is the following: <br>

<pre>   (start-time end-time brief-description long-description color url) </pre>

Start-time and end-time are integers (a number of seconds since january 1, 1970).
The function time-encoding from the <a href=\"../../../../lib/man/time.html\">time library</a> is useful for construction of these
numbers.<p>

Brief-description is a string, the contents of which goes into the calendar.
Long-description is a longer description of the event. Currently, this information is not used. <p>

Color is a RGB color list (a list of three integers). You can use the color constans of the html Scheme library,
if you want to. <p>

Finally, url is the url, to which we link from the calendar entry. If it is the empty string, no link will be made.



")) ((kind "manual-section") (section-title "Event selection functions.") (section-body "
The event selector functions define accessors into a calendar appointment.

")) ((kind "manual-page") (description "Return the start time of a calendar appointment.
") (title "event-start-time") (form (event-start-time e))) ((kind "manual-page") (description "Return the end time of a calendar appointment.
") (title "event-end-time") (form (event-end-time e))) ((kind "manual-page") (description "Return the brief textual description of a calendar appointment.
") (title "event-brief-text") (form (event-brief-text e))) ((kind "manual-page") (description "Return the long textual description of a calendar appointment.
") (title "event-long-text") (form (event-long-text e))) ((kind "manual-page") (description "Return the  color of a calendar appointment.
") (title "event-color") (form (event-color e))) ((kind "manual-page") (description "Return the url of a calendar appointment.
") (title "event-url") (form (event-url e))) ((kind "manual-section") (section-title "Calendar parameters.") (section-body "
In this section we find a number of variables which control the calendar appearance.

")) ((kind "manual-page") (description "Week separators: Whether thin og thick (symbols).
") (title "week-separator-of-calendar") (form week-separator-of-calendar)) ((kind "manual-page") (description "The width of a month. Default value is 150.
") (title "month-width") (form month-width)) ((kind "manual-page") (description "The background color of the calendar.
A color can be constructed by use of the function make-color from the LAML color library. 
The default background color is white.
") (title "calendar-background-color") (form calendar-background-color)) ((kind "manual-section") (section-title "The main calendar function.") (section-body "
In this section we find the most important function at all, the calendar function which generates a calendar.

")) ((kind "manual-page") (cross-references (internal-references "calendar events" "SECTION1")) (description "Return a HTML calendar for year, from-months, and running through number-of-months. 
This is the absolute main function of the tool. 
As an example, (calendar 1999 2 3) will produce a calendar of February, March, and April in 1999.
If the optional parameter show-today is #t, the current date is shown in red.") (pre-condition "The variable calendar-events must be defined prior to the call of this function") (title "calendar") (form (calendar year from-month number-of-months . show-today))))