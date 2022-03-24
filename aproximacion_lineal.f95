module aproximacion
    use lineal 

    contains 
    

    subroutine minimos_cuadrados(A,B,x,xn,yn)
        real(8),intent(in) :: Xn(:), Yn(:)
        real(8), intent(inout):: A(:,:), X(:)
        real(8), intent(inout):: B(:)
        integer :: i,l,j,k,n,m
        real(8)::s, s2

        n = size(xn)
        m = size(x)-1
        
        
        write(*,*)'mnmnmn',n,m
        do l=0, m
            do k=0, m 
                s2 = 0.d0 
                do i =1, n 
                    s2 = s2 + (xn(i)**l)*(xn(i)**k)
                    
                enddo 
                A(l+1,k+1) = s2
            enddo 
        enddo 
       
        do l=0, m
            s=0.d0
            do i=1, n 
               
                s = s + (xn(i)**l) * yn(i)
                ENDDO
            b(l+1)=s 
        enddo
       
        call gauss(A,B,x)

            write(*,*) '------X-------'
           write(*,*) X 
    
    end subroutine minimos_cuadrados


    subroutine error(Xn,Yn,px,X)
        real(8), intent(in)::Xn(:),Yn(:),X(:)
        real(8), intent(out)::px
        integer::m,n,i
        real(8)::s  

        n=size(xn)
        m= size(x)-1
        s=0.d0

        do i=0, m
            s= s + X(i+1)*xn(i+1)**i 
            px=s 
        enddo 
        write(*,*)'-------PX-------'
        write(*,*)PX 

    end subroutine error 


end module aproximacion