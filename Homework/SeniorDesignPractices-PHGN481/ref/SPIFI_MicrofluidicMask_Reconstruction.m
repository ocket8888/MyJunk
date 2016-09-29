%% Reconstruction for SPIFI Microfluidic Mask Imaging
%
% This is for the quadratic chirp-based mask
%
% Randy Bartels
% Colorado State University
% Dec. 14, 2013
%
% round 2 edited on Dec. 15: trying phase shifting for processing
% housekeeping
%
% new version May 1, 2014

clc
clear

% load simulation
load MicrofluidicSPIFISimulations

%% Regenerate plots


% plot all mode
sw_plotall = 1;
plt_ctr=0;

% plot the mask
if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    imagesc(x/um,y/mm,Mask1)
    colormap gray
    xlabel('x [um]')
    ylabel('y [mm]')
    title('Mask 1')
    %axis equal
    
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    imagesc(x/um,y/mm,Mask2)
    colormap gray
    xlabel('x [um]')
    ylabel('y [mm]')
    title('Mask 2')
    %axis equal
    
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    imagesc(x/um,y/mm,Mask3)
    colormap gray
    xlabel('x [um]')
    ylabel('y [mm]')
    title('Mask 3')
    %axis equal
    
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    imagesc(x/um,y/mm,OBJc)
    colormap gray
    xlabel('x [um]')
    ylabel('y [mm]')
    title('Object Contrast')
    axis square
    
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(YC/mm,Corr1,YC/mm,Corr2,YC/mm,Corr3)
    xlabel('y [mm]')
    ylabel('s_j(y)')
    title('Signals from the three masks')
    legend('Mask1','Mask2','Mask3')
    
end


%%
% plot all mode
plt_ctr=300;


% look at the x and y projections

% x projection of the object
xproj = trapz(x,abs(OBJc).^2,2); xproj = xproj/max(xproj);

% y projection of the object
yproj = trapz(y,abs(OBJc).^2,1); yproj = yproj/max(yproj);

if (sw_plotall)
    
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(y/mm,xproj)
    xlabel('y [mm]')
    ylabel('x_{proj}')
    title('Scan-direction image')

    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(x/mm,yproj)
    xlabel('x [mm]')
    ylabel('y_{proj}')
    title('Chirped-like image')


end


%%
% plot all mode
plt_ctr=800;
New_Plts = 200;


%% We'll reconstruct the SPIFI-inspired mask (Mask1)
% 

% lop off anything not in our mask range
JJ = find(abs(YC)<0.9*L/2);
Corr1 = Corr1 - mean(Corr1(JJ));
Corr1=Corr1.*(abs(YC)<0.9*L/2);
if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(YC/mm,Corr1)
    ylabel('y [mm]')
    title('Signal Correlation')
end

SCorr1 = fftshift(fft(fftshift(Corr1)));

dfyc = 1/2/Ycorr;

fyc = ([1:Nc]-Nc/2)*dfyc;


% plot the frequency domain to get the y projections resolved spatially
% along x
if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(fyc*mm,abs(SCorr1))
    xlabel('fyc [1/mm]')
    title('Signal Correlation Spectrum')
    axis([-15 15 0 max(abs(SCorr1))])
end

%% Attempt to correct the nonlinear modulation frequency to spatial mapping

% here is the mapping that we found in the Mathematica calculation
% x=a(1-(a \[Kappa])/(Subscript[f, yc] Subscript[v, max]))
%
% This is only for positive y spatial frequency values
%
% so, to use this, we take the correlated y spatial frequency (fyc), and
% remap it to the space, then we can use spline to remap it in a linear way
% if we like, but we need only do a change of plot initially

JJ = find(fyc>0);
fycP = fyc(JJ);
SCorr1P = SCorr1(JJ);

xcSB = a*(1-(a*kappa)./(fycP*vmax));
GG = find(abs(xcSB)<R*a);
xcSBChannel = xcSB(GG);
JacCorr1 = a^2*kappa/vmax*(fycP(GG)).^4; % apply the Jacobian correction to scale amplitude correctly (2.35 power seems to work)
%JacCorr1 = 1;
SCorr1Channel = SCorr1P(GG);
% correct energy density in the image
SCorr1ChannelJ=SCorr1Channel.*JacCorr1;

% check this mapping by computing the centroid of the y-direction FFT
Mask1yFFT=fftshift(fft(fftshift(Mask1-mean(mean(Mask1))),Ny,1));
% blank the zero order and negative frequencies
Mask1yFFT(1:floor(Ny*0.51),:) = 0;


[FX,FY]=meshgrid(fx,fy);

if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    imagesc(abs(Mask1yFFT))
    colormap gray
end

% centroid of the modulation frequency (% no longer works with the harmonic
% content, so get the peak values
centymod = trapz(fy,FY.*Mask1yFFT,1)./trapz(fy,Mask1yFFT,1);
[MaxValues,IndexCentymod]=max(abs(Mask1yFFT));
centymod=fy(IndexCentymod);

% make a plot of the extracted frequency mapping compared to the values that are expected
if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(xcSBChannel/um,fycP(GG)*mm,x/um,real(centymod)*mm,'linewidth',2)
    xlabel('x [um]')
    ylabel('modulation frequency with position [1/mm]')
    title('Computed and Calculated Modulation Frequency Mapping')
end


% compare the recovered image to the expected y projection
if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(xcSBChannel/um,abs(SCorr1ChannelJ)/max(abs(SCorr1ChannelJ)),x/um,yproj,'-.','linewidth',2)
    xlabel('xyc [um]')
    title('Signal Correlation Spectrum: REMAPPED')
    axis([-R*a/um R*a/um 0 1])
end

% notice the second harmonic in the images.

%% Now, the y(chirped direction)-chirped mask

% Compute the mask spectral filter
Mask2Oscillatory = cos(beta.*((YC+L/2).^2)).*(abs(YC/L)<0.5);

if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(YC/mm,Mask2Oscillatory)
    xlabel('yc [mm]')
    title('AC part of y-chirped mask')
    axis([-Dy/2/mm Dy/2/mm -1 1])
end

Mask2OscillatorySpec = fftshift(fft(fftshift(Mask2Oscillatory)));
PhaseFilter = angle(Mask2OscillatorySpec);
MagFilter = abs(Mask2OscillatorySpec);MagFilter = MagFilter/max(MagFilter);


if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    subplot(211)
    plot(fyc*mm,MagFilter)
    xlabel('fyc [mm^{-1}]')
    title('|Filter|')
%    axis([-Dy/2/mm Dy/2/mm -1 1])
    subplot(212)
    plot(fyc*mm,PhaseFilter)
    xlabel('fyc [mm^{-1}]')
    title('Filter Phase')
%    axis([-Dy/2/mm Dy/2/mm -1 1])
end

JJ = find(MagFilter>0.5);
ComplexFilter = ones(size(MagFilter));
ComplexFilter(JJ) = 1./MagFilter(JJ);
ComplexFilter = ComplexFilter.*exp(- 1i*PhaseFilter);

if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    subplot(211)
    plot(YC/mm,abs(ComplexFilter))
    xlabel('yc [mm]')
    title('|Filter|')
%    axis([-Dy/2/mm Dy/2/mm -1 1])
    subplot(212)
    plot(YC/mm,angle(ComplexFilter))
    xlabel('yc [mm]')
    title('Filter Phase')
%    axis([-Dy/2/mm Dy/2/mm -1 1])
end

if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(YC/mm,phase(ComplexFilter.*Mask2OscillatorySpec))
    xlabel('yc [mm]')
    title('Check filter phase product')
    axis([-Dy/2/mm Dy/2/mm -1 1])
end


% remove the dc component from the signal
Corr2AC = (Corr2 - mean(Corr2(find((abs(YC+.075*L)<0.5))))).*(abs((YC+.075*L)/L)<0.5);


if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(YC/mm,Corr2,YC/mm,Corr2AC)
    xlabel('yc [mm]')
    title('AC Signal Component')
%    axis([-Dy/2/mm Dy/2/mm -1 1])
end

% spectrum of the signal
SpecCorr2AC = fftshift(fft(fftshift(Corr2AC)));

if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    subplot(211)
    plot(fyc*mm, abs(SpecCorr2AC))
    xlabel('fyc [mm^{-1}]')
    title('Signal Spectral Magnitude')
%    axis([-Dy/2/mm Dy/2/mm -1 1])
    subplot(212)
    plot(fyc*mm, angle(SpecCorr2AC))
    xlabel('fyc [mm^{-1}]')
    title('Signal Spectral Phase')
end

% filtered signal spectrum
FilteredSignalSpectrum = SpecCorr2AC.*ComplexFilter;

% reconstrcuted object
ReconObject = abs(fftshift(fft(fftshift(FilteredSignalSpectrum))));
ReconObject = ReconObject/max(ReconObject);

if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(YC/mm,abs(ReconObject),y/mm,xproj)
    xlabel('yc [mm]')
    title('Y chirp mask reconstruction')
    axis([-Dy/2/mm Dy/2/mm 0 1])
end
