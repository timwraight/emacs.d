;;UK public holidays, and other UK notable dates.
(setq general-holidays
      '((holiday-fixed 1 1 "New Year's Day")
        (holiday-new-year-bank-holiday)
        (holiday-fixed 2 14 "Valentine's Day")
        (holiday-fixed 3 17 "St. Patrick's Day")
        (holiday-fixed 4 1 "April Fools' Day")
        (holiday-easter-etc -47 "Shrove Tuesday")
        (holiday-easter-etc -21 "Mother's Day")
        (holiday-easter-etc -2 "Good Friday")
        (holiday-easter-etc 0 "Easter Sunday")
        (holiday-easter-etc 1 "Easter Monday")
        (holiday-float 5 1 1 "Early May Bank Holiday")
        (holiday-float 5 1 -1 "Spring Bank Holiday")
        (holiday-float 6 0 3 "Father's Day")
        (holiday-float 8 1 -1 "Summer Bank Holiday")
        (holiday-fixed 10 31 "Halloween")
        (holiday-fixed 12 24 "Christmas Eve")
        (holiday-fixed 12 25 "Christmas Day")
        (holiday-fixed 12 26 "Boxing Day")
        (holiday-christmas-bank-holidays)
        (holiday-fixed 12 31 "New Year's Eve")))

;;Major US holidays
(setq other-holidays
      '((holiday-float 1 1 3 "Martin Luther King Day")
        (holiday-float 2 1 3 "President's Day")
        (holiday-float 5 1 -1 "Memorial Day")
        (holiday-fixed 7 4 "Independence Day")
        (holiday-float 9 1 1 "Labor Day")
        (holiday-float 10 1 2 "Columbus Day")
        (holiday-fixed 11 11 "Veteran's Day")
        (holiday-float 11 4 4 "Thanksgiving")))

;;N.B. It is assumed that 1 January is defined with holiday-fixed -
;;this function only returns any extra bank holiday that is allocated
;;(if any) to compensate for New Year's Day falling on a weekend.
;;
;;Where 1 January falls on a weekend, the following Monday is a bank
;;holiday.
(defun holiday-new-year-bank-holiday ()
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y 1)
    (when (<= m 3)
      (let ((d (calendar-day-of-week (list 1 1 y))))
        (cond ((= d 6)
               (list (list (list 1 3 y)
                           "New Year's Day Bank Holiday")))
              ((= d 0)
               (list (list (list 1 2 y)
                           "New Year's Day Bank Holiday"))))))))

;;N.B. It is assumed that 25th and 26th are defined with holiday-fixed -
;;this function only returns any extra bank holiday(s) that are
;;allocated (if any) to compensate for Christmas Day and/or Boxing Day
;;falling on a weekend.
;;
;;Christmas day is always 25 December; beyond that there is no
;;entirely consistent practice.  We proceed as follows:
;;
;;Traditionally, Boxing day was the first day after Christmas, not
;;including Sundays (i.e. if Christmas fell on a Saturday, Boxing Day
;;was Monday 27th) however we follow modern practice here and always
;;regard Boxing Day as 26 December (which, as noted above, is never
;;returned by this function).
;;
;;Generally the extra bank holiday is allocated on the first available
;;day that would otherwise have been a working day.  However in the
;;case where we need to allocate two additional bank holidays -
;;i.e. where Christmas Day falls on the Saturday, there is some
;;confusion as to how to proceed.  We allocate the Boxing Day Bank Holiday
;;to the Monday, since this is the historic date of Boxing Day in this
;;case, and allocate the Christmas Day Bank Holiday to the following day.
;;
;;This is consistent with the way that the 'substitute days' were
;;allocated in the list of bank holidays on the Department of Trade
;;and Industry in the recent past, although they don't use the any
;;specific names for these holidays.
;;
;;The latest list on the direct.gov.uk web site is not consistent with
;;this practice, however, allocating the substitute days for Christmas
;;Day and Boxing Day in the other order in 2010.  However this list
;;also manages to allocate them in order in 2011 (where Christmas Day
;;falls on a Sunday), therefore placing the substitute holiday for
;;Christmas Day _on_ Boxing Day, and then the substitute holiday for
;;Boxing Day on the following day.  I'm not at all sure this isn't a
;;mistake.
;;
;;In any case, this is largely academic as there is no dispute over
;;which days are public holidays, only what to call them - so unless
;;you care deeply just ignore the issue and use the function as
;;supplied.
(defun holiday-christmas-bank-holidays ()
  (let ((m displayed-month)
        (y displayed-year))
    (increment-calendar-month m y -1)
    (when (>= m 10)
      (let ((d (calendar-day-of-week (list 12 25 y))))
        (cond ((= d 5)
               (list (list (list 12 28 y)
                           "Boxing Day Bank Holiday")))
              ((= d 6)
               (list (list (list 12 27 y)
                           "Boxing Day Bank Holiday")
                     (list (list 12 28 y)
                           "Christmas Day Bank Holiday")))
              ((= d 0)
               (list (list (list 12 27 y)
                           "Christmas Day Bank Holiday"))))))))

;;Comment out the Christian holidays that also have secular
;;significance in the UK (Shrove Tuesday, Good Friday, Easter Sunday,
;;Christmas) as EMACS doesn't remove duplicates holidays.  These
;;holidays are included in the UK redefinition of general-holidays
;;(where Chistmas is listed as Christmas Day).
(setq christian-holidays
      '((if all-christian-calendar-holidays
            (holiday-fixed 1 6 "Epiphany"))
                                        ;	(holiday-easter-etc 0 "Easter Sunday")
                                        ;	(holiday-easter-etc -2 "Good Friday")
        (holiday-easter-etc -46 "Ash Wednesday")
        (if all-christian-calendar-holidays
            (holiday-easter-etc -63 "Septuagesima Sunday"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc -56 "Sexagesima Sunday"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc -49 "Shrove Sunday"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc -48 "Shrove Monday"))
                                        ;	(if all-christian-calendar-holidays
                                        ;	    (holiday-easter-etc -47 "Shrove Tuesday"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc -14 "Passion Sunday"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc -7 "Palm Sunday"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc -3 "Maundy Thursday"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc 35 "Rogation Sunday"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc 39 "Ascension Day"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc 49 "Pentecost (Whitsunday)"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc 50 "Whitmonday"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc 56 "Trinity Sunday"))
        (if all-christian-calendar-holidays
            (holiday-easter-etc 60 "Corpus Christi"))
        (if all-christian-calendar-holidays
            (holiday-greek-orthodox-easter))
        (if all-christian-calendar-holidays
            (holiday-fixed 8 15 "Assumption"))
        (if all-christian-calendar-holidays
            (holiday-advent 0 "Advent"))
                                        ;	(holiday-fixed 12 25 "Christmas")
        (if all-christian-calendar-holidays
            (holiday-julian 12 25 "Eastern Orthodox Christmas"))))


(setq
 holiday-hebrew-holidays nil
 holiday-islamic-holidays nil
 holiday-bahai-holidays nil
 holiday-oriental-holidays nil
 holiday-solar-holidays nil)
