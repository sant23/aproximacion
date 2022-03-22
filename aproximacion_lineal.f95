module aproximacion
    use lineal 

    contains 
    

    subroutine minimos_cuadrados(A,B,x,xn,yn)
        real(8), allocatable,intent(in) :: Xn(:), Yn(:)
        real(8), allocatable, intent(inout):: A(:,:), X(:)
        real(8), allocatable, intent(inout):: B(:)
        integer :: i,l,j,k,n,m
        real(8)::s, s2

        do l=0, m
            do k=0, m 
                s2 = 0.d0 
                do i =1, n 
                    s2 = s2 + (xn(i)**l)*(yn(i)**k)
                    A(l,k) = s2
                enddo 
            enddo 
        enddo 
        
        do l=0, m
            s=0.d0
            do i=1, n 
                b(l+1)= + xn(i)**l * yn(i)
                
            ENDDO
            
        enddo
       

        
    
    end subroutine minimos_cuadrados

    subroutine error

    end subroutine error 


end module aproximacion