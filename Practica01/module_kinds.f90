module kinds
    implicit none

    integer,parameter :: ikind1 = selected_int_kind(1)
    integer,parameter :: ikind2 = selected_int_kind(3)
    integer,parameter :: ikind4 = selected_int_kind(5)
    integer,parameter :: ikind8 = selected_int_kind(10)
    integer,parameter :: ikind16 = selected_int_kind(19)

    real,parameter :: rkind4 = selected_real_kind(1)
    real,parameter :: rkind8 = selected_real_kind(7)
    real,parameter :: rkind10 = selected_real_kind(16)
    real,parameter :: rkind16 = selected_real_kind(19)
end module kinds