module my_funcs
    use kinds
    implicit none

    integer,parameter :: order = 6
    contains

    function F(T,O)
        implicit none
        real(rkind8) :: F(order)
        real(rkind8),intent(in) :: T(:),O(:)

        integer :: w1=10,w2=20,L1=3,L2=4,L3=4,L=8
        real(rkind8) :: g=9.8

        F(1) = T(1)*sin(O(1)) - T(2)*sin(O(2)) - w1
        F(2) = T(1)*cos(O(1)) - T(2)*cos(O(2))
        F(3) = T(2)*sin(O(2)) + T(3)*sin(O(3)) - w2
        F(4) = T(2)*cos(O(2)) - T(3)*cos(O(3))
        F(5) = L1*cos(O(1)) + L2*cos(O(2)) + L3*cos(O(3)) - L
        F(6) = L1*sin(O(1)) + L2*sin(O(2)) - L3*sin(O(3))

    end function

    function jacobiano(T,O)
        implicit none
        real(rkind8) :: jacobiano(order,order)
        real(rkind8),intent(in) :: T(:),O(:)

        integer :: L1=3,L2=4,L3=4

        jacobiano(1,1) = sin(O(1))
        jacobiano(1,2) = -sin(O(2))
        jacobiano(1,3) = 0
        jacobiano(1,4) = T(1)*cos(O(1))
        jacobiano(1,5) = -T(2)*cos(O(2))
        jacobiano(1,6) = 0

        jacobiano(2,1) = cos(O(1))
        jacobiano(2,2) = -cos(O(2))
        jacobiano(2,3) = 0
        jacobiano(2,4) = -T(1)*sin(O(1))
        jacobiano(2,5) = T(2)*sin(O(2))
        jacobiano(2,6) = 0

        jacobiano(3,1) = 0
        jacobiano(3,2) = sin(O(2))
        jacobiano(3,3) = sin(O(3))
        jacobiano(3,4) = 0
        jacobiano(3,5) = T(2)*cos(O(2))
        jacobiano(3,6) = T(3)*cos(O(3))

        jacobiano(4,1) = 0
        jacobiano(4,2) = cos(O(2))
        jacobiano(4,3) = -cos(O(3))
        jacobiano(4,4) = 0
        jacobiano(4,5) = -T(2)*sin(O(2))
        jacobiano(4,6) = T(3)*sin(O(3))

        jacobiano(5,1) = 0
        jacobiano(5,2) = 0
        jacobiano(5,3) = 0
        jacobiano(5,4) = -L1*sin(O(1))
        jacobiano(5,5) = -L2*sin(O(2))
        jacobiano(5,6) = -L3*sin(O(3))

        jacobiano(6,1) = 0
        jacobiano(6,2) = 0
        jacobiano(6,3) = 0
        jacobiano(6,4) = L1*cos(O(1))
        jacobiano(6,5) = L2*cos(O(2))
        jacobiano(6,6) = -L3*cos(O(3))
    end function

    function G(p)
        implicit none
        real(rkind8) :: G(order)
        real(rkind8),intent(in) :: p(:) !p(1) : valor de x, p(2): valor de y
        
        G(1) = (6 - p(3) - p(1)**2 - p(2)**2)/(-2)
        G(2) = (-62 + p(1)**3 + p(1)**2 - 3*p(1) + p(2)**2 + 10*p(3))/2
        G(3) = (65 + p(1)**3 - p(1)**2 - p(2)**2 + 2*p(2))/12

    end function
    
    function G_gauss_seidel(p) result(G)
        implicit none
        real(rkind8) :: G(order)
        real(rkind8),intent(in) :: p(:) !p(1) : valor de x, p(2): valor de y, p(3): z

        G(1) = (6 - p(3) - p(1)**2 - p(2)**2)/(-2)
        G(2) = (-62 + G(1)**3 + G(1)**2 - 3*G(1) + p(2)**2 + 10*p(3))/2
        G(3) = (65 + G(1)**3 - G(1)**2 - G(2)**2 + 2*G(2))/12

    end function

    function deriv_parciales(p) result(dG)
        implicit none
        real(rkind8) :: dG(order,order)
        real(rkind8),intent(in) :: p(:)

        dG(1,1) = p(1) !Derivada de la función G(1) respecto de x
        dG(1,2) = -0.5 !Derivada de la función G(1) respecto de y
        dG(2,1) = -0.25*p(1) !Derivada de la función G(2) respecto de x
        dG(2,2) = -p(2) + 1 !Derivada de la función G(2) respecto de y

    end function
end module