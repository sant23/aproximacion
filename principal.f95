program minimos
    use lineal
    use aproximacion 

    implicit none
    real(8), allocatable :: Xn(:), Yn(:), A(:,:), B(:), X(:)
    real(8)::px
    character(15)::fichero 
    integer :: i,l,j,k,n,m
    

    
    
    print*, 'Introduzca el valor de m'
    read*, m 
  
    
    fichero = 'datos.dat'
    open(unit=10, file = fichero)
   
    read(10,*) n
   
    allocate (Xn(n),Yn(n), A(m+1,m+1), B(m+1), X(m+1))

    do i=1, n   
        read(10,*) Xn(i), Yn(i)
    ENDDO 

    call minimos_cuadrados(a,b,x,xn,yn)

    
     write(*,*) '-------b--------'
     write(*,*)   b  
    write(*,*) '-------a--------'
    write(*,*) a 


    !write (*,*) '------vector xn-------'
   ! write(*,*) xn
    !write (*,*) '------vector yn-------'
    !write(*,*) yn
    
    call error(Xn,Yn,px,x)


end program  minimos