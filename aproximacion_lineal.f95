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
        
        
        write(*,*)'n----m',n,m
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


    subroutine error(Xn,Yn,px,X,e)
        real(8), intent(in)::Xn(:),Yn(:),X(:)
        real(8), intent(out)::e,px(:)
        integer::m,n,i
        real(8)::s,s2  

        n=size(xn)
        m= size(x)-1
        
        
       ! e=0.d0
        !do i=1, n
         !       s=0.d0
        !        do l=0, m 
        !            s= s + X(l+1)*xn(i)**(l) 
        !            px(i)=s
         !           ENDDO
                
        !        e = e + (yn(i)-s)**2
       ! enddo
        
        !write(*,*)'-------PX-------'
        !write(*,*)px
        !write(*,*)'------ERROR-----'
        !write(*,*) e

    end subroutine error 


end module aproximacion