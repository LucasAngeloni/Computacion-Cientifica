module kinds
    implicit none

    integer,parameter :: ikind1 = selected_int_kind(1)
    integer,parameter :: ikind2 = selected_int_kind(3)
    integer,parameter :: ikind4 = selected_int_kind(5)
    integer,parameter :: ikind8 = selected_int_kind(10)
    integer,parameter :: ikind16 = selected_int_kind(19)

    integer,parameter :: rkind4 = selected_real_kind(6)
    integer,parameter :: rkind8 = selected_real_kind(15)
    integer,parameter :: rkind10 = selected_real_kind(18)
    integer,parameter :: rkind16 = selected_real_kind(33)
end module kinds