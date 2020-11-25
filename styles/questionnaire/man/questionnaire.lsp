(((kind "manual-section") (section-title "Front matters.") (section-body "The functions and variables in this section are front matters to the real questionnaire.

")) ((kind "manual-page") (description "Hidden answers. In some situations we might want (in an adhoc fashion) to
hide certain answers. The answers to the questions ids in hidden-ids are not shown. Intial value is the empty list.
") (title "hidden-ids") (form hidden-ids)) ((kind "manual-page") (description "Is the submitted questionnaire self contained?
") (title "questionnaire-self-contained") (form questionnaire-self-contained)) ((kind "manual-page") (description "The questionnaire color scheme. 
A list of four colors: background, foreground, link, visited links colors.
") (title "questionnaire-color-scheme") (form questionnaire-color-scheme)) ((kind "manual-page") (description "The font size of a questionnaire
") (title "questionnaire-font-size") (form questionnaire-font-size)) ((kind "manual-page") (description "The width of an identification question field (in terms of number of characters)
") (title "identification-question-width") (form identification-question-width)) ((kind "manual-page") (description "The width of an free-style question field (in terms of number of characters)
") (title "free-style-question-width") (form free-style-question-width)) ((kind "manual-page") (description "The height of an free-style question field (in terms of number of characters)
") (title "free-style-question-height") (form free-style-question-height)) ((kind "manual-page") (description "Question emphasis function. This function is applied on any question text.
") (title "question-emphasis-function") (form question-emphasis-function)) ((kind "manual-section") (section-title "Questionnaire forms.") (section-body "In this section you will find the top-level questionnaire form together with possible subforms.

")) ((kind "manual-page") (description "Write a questionnaire to a file.
The title is the title of the questionnaire.
receiving-program-url is the URL of the CGI program which processes the answers.
questionnaire-id is a symbol which identifies this particular questionnaire.
The question-list is a number of forms: identification-question, free-style-question, multi-choice-question, rating-question.
") (title "questionnaire") (form (questionnaire title receiving-program-url questionnaire-id . question-list))) ((kind "manual-page") (cross-references (internal-references "Alternative form" "free-style-question")) (description "Ask a question which requires a one line answer.
The answer to this question is meant to identify a particular answer relative to other answers.
Thus, typically ask for the name of the submitter in an identification question.
Id is the identification of this question (a symbol).") (title "identification-question") (form (identification-question id question))) ((kind "manual-page") (description "Ask a question which requires a multi-line answer.
The id is the identification of this question (a symbol).
") (title "free-style-question") (form (free-style-question id question))) ((kind "manual-page") (description "Ask a multi-choice question.
Possibilities is a list of possible answers.
One or more answers can be selected.
The id is the identification of this question (a symbol).
") (title "multi-choice-question") (form (multi-choice-question id question possibilities))) ((kind "manual-page") (description "Ask a rating question.
The question can be answered by selecting exactly one of answers presenting in the rating-list.
Gives entry (\"id\" . \"n\"), where n is the entry selected.
The first is number 1.
The id is the identification of this question (a symbol).
Rating-list is a list of strings.
") (title "rating-question") (form (rating-question id question rating-list))) ((kind "manual-page") (description "As a question which can be answered by choosing one answer among a selection of possible answers.
Same form as rating question. However, the ordering of the answers is not important for single-choice-question.") (form "(single-choice-question id question possible-answers)") (title "single-choice-question")) ((kind "manual-page") (description "Define the feedback message, which appears when the questionnaire form is submitted.
Do NOT use the characters '{', '}', and '£' in the message. These
characters are used for encoding purposes of embedded HTML anchor fragments - ad hoc stuff.
") (title "feedback-message") (form (feedback-message message))) ((kind "manual-section") (section-title "CGI relevant stuff.") (section-body "In sub-directory cgi-support of the directory holding the questionnaire.scm file, there is a number
of CGI Scheme programs, which accepts answers and display the results of the questioning It is up to
the administrator of the questionnaire facility (the LAML installer) to arrange that actual
cgi programs are set up in an appropriate cgi-bin directory. These actual CGI programs can just load
the Scheme programs in cgi-support.<p>
The CGI stuff is not part of the LAML distribution.

")) ((kind "manual-page") (description "show txt in a colored answer box
") (title "answer-box") (form (answer-box txt))) ((kind "manual-page") (description "Return a symbol assembled from constituents
") (title "syn-con") (form (syn-con . constituents))) ((kind "manual-page") (description "Return a formatted answer with id and type.
") (title "format-answer") (form (format-answer answer-record id type))) ((kind "manual-page") (description "Present the answer in answer-record
") (title "show-an-answer") (form (show-an-answer answer-record))))