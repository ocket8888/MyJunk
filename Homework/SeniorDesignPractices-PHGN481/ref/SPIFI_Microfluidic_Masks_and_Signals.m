%% Model for SPIFI Microfluidic Mask Imaging
%
% This is for the quadratic chirp-based mask
%
% Randy Bartels
% Colorado State University
% Dec. 14, 2013
%
% Truncated for mask and s(t) generation: Jan. 31, 2014
%
% Edited May 1, 2014
%

clc
clear

% plot all mode
sw_plotall = 1;
plt_ctr=0;
New_Plts = 200;

% Units
mm = 1e-3;  % define mm
cm = 1e-2;  % define cm
um = 1e-6;  % define microns
nm = 1e-9;  % define nanometers
Hz = 1;     % define Hz
ms = 1e-3;  % define ms
us = 1e-6;  % define us
s = 1;      % defind s

% Set up mask parameters
beta = 30/mm^2; % chirp rate of the mask
a = 250*um; % Half width of the channel
R = 1; % frational width of the mask
L = 5*mm; % length of the mask along the flow direction
kappa = 350*Hz/mm; % chirp rate of the mask
R = 0.6; % frational width of the mask
vmax = 0.75*cm/s; % peak flow velocity

% x-direction spatial grid
Dx = 0.55*mm; % length of x window
Nx = 2^12;   % number of x sample points
dx = Dx/(Nx-1); % x sample spacing
x = ([0:Nx-1]-Nx/2)*dx; % generate the x vector

% y-direction spatial grid
Dy = L+.6*mm; % length of y window
Ny = 2^12;   % number of y sample points
dy = Dy/(Ny-1); % y sample spacing
y = ([0:Ny-1]-Ny/2)*dy; % generate the y vector

% spatial frequency vectors
dfx = 1/Nx/dx; % fx spacing
fx = ([0:Nx-1]-Nx/2)*dfx; % generate the fx vector
dfy = 1/Ny/dy; % fy spacing
fy = ([0:Ny-1]-Ny/2)*dfy; % generate the fy vector


% make matrix versions of x and y
[X,Y] = meshgrid(x,y);


%% Generate Mask 1 (the x-chirped mask)

Mask1 = (1/2+1/2*cos(2*pi*a*kappa/vmax./(1-X/a).*Y)).*(abs(X/a)<R).*(abs(Y/L)<0.5);
Mask1(isnan(Mask1)) = 0; % get rid of NaNs
%make binary
JJ= find(Mask1 > 0.5);
Mask1 = zeros(size(Mask1));
Mask1(JJ)=1;

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

end

%% Generate Mask 2 (the y-chirped mask)

Mask2 = (1/2+1/2*cos(beta.*((Y+L/2).^2))).*(abs(X/a)<R).*(abs(Y/L)<0.5);
Mask2(isnan(Mask2)) = 0; % get rid of NaNs
JJ= find(Mask2 > 0.5);
Mask2 = zeros(size(Mask2));
Mask2(JJ)=1;

% plot the mask
if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    imagesc(x/um,y/mm,Mask2)
    colormap gray
    xlabel('x [um]')
    ylabel('y [mm]')
    title('Mask 2')
    %axis equal

end


%% Generate Mask 3 (the combo mask)

Mask3 = (1/2+1/2*cos(2*pi*a*kappa/vmax./(1-X/a).*Y)).*cos(beta.*((Y+L/2).^2)).*(abs(X/a)<R).*(abs(Y/L)<0.5);
Mask3(isnan(Mask3)) = 0; % get rid of NaNs
%make binary
JJ= find(Mask3 > 0.5);
Mask3 = zeros(size(Mask3));
Mask3(JJ)=1;

% plot the mask
if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    imagesc(x/um,y/mm,Mask3)
    colormap gray
    xlabel('x [um]')
    ylabel('y [mm]')
    title('Mask 3')
    %axis equal

end


%% Create the object (2D) and correlate to create the signal

Ycorr = .5*Dy; % half range for the correlation
Nc = Ny; % number of y correlation points
YC = linspace(-Ycorr,Ycorr,Nc); % y correlation vector

% be able to switch between objects
%obj_sw = 'cell';
%obj_sw = 'delta';
obj_sw = 'several blobs';
%obj_sw = '';

clear Corr1;
clear Corr2;
clear Corr3;

switch obj_sw
    case 'cell'
        sig = 25*um;

        for jj = 1:Nc
            OBJ = exp(-(X.^2+(Y-YC(jj)).^2)/sig^2)+exp(-(X.^2+(Y-YC(jj)).^2)/sig^2);
            Corr1(jj)=trapz(x,trapz(y,abs(OBJ.*Mask0)).^2);
            Corr2(jj)=trapz(x,trapz(y,abs(OBJ.*Mask1)).^2);
            Corr3(jj)=trapz(x,trapz(y,abs(OBJ.*Mask2)).^2);

            if (jj==floor(Nc/2))
                if (sw_plotall)
                    OBJc=OBJ;
                    plt_ctr = plt_ctr+1;
                    figure(plt_ctr)
                    imagesc(x/um,y/mm,OBJ)
                    colormap gray
                    xlabel('x [um]')
                    ylabel('y [mm]')
                    title('Object Contrast')
                    axis square
                end
            end
        end
        
        
    case 'delta'
        sig = 35*um;
        
        for jj = 1:Nc
            OBJ = exp(-(X.^2+(Y-YC(jj)).^2)/sig^2);
            Corr1(jj)=trapz(x,trapz(y,abs(OBJ.*Mask1)).^2);
            Corr2(jj)=trapz(x,trapz(y,abs(OBJ.*Mask2)).^2);
            Corr3(jj)=trapz(x,trapz(y,abs(OBJ.*Mask3)).^2);
            if (jj==floor(Nc/2))
                if (sw_plotall)
                    OBJc=OBJ;
                    plt_ctr = plt_ctr+1;
                    figure(plt_ctr)
                    imagesc(x/um,y/mm,OBJ)
                    colormap gray
                    xlabel('x [um]')
                    ylabel('y [mm]')
                    title('Object Contrast')
                    axis square
                end
            end
            
        end
        
    case 'several blobs'
        sig = 35*um;
        W=120*um;
        Sep = 120*um;
        
        for jj = 1:Nc
            OBJ = exp(-(X.^2+(Y-W-YC(jj)).^2)/sig^2)+exp(-(X.^2+(Y+1.5*W-YC(jj)).^2)/(sig/2)^2)+exp(-((X+Sep).^2+(Y-YC(jj)).^2)/(sig/2)^2);
            Corr1(jj)=trapz(x,trapz(y,abs(OBJ.*Mask1)).^2);
            Corr2(jj)=trapz(x,trapz(y,abs(OBJ.*Mask2)).^2);
            Corr3(jj)=trapz(x,trapz(y,abs(OBJ.*Mask3)).^2);
            if (jj==floor(Nc/2))
                if (sw_plotall)
                    OBJc=OBJ;
                    plt_ctr = plt_ctr+1;
                    figure(plt_ctr)
                    imagesc(x/um,y/mm,OBJ)
                    colormap gray
                    xlabel('x [um]')
                    ylabel('y [mm]')
                    title('Object Contrast')
                    axis square
                end
            end
        end
        
    otherwise
        Tobj=ones(size(X));                 % no object
end


%% plot the signal traces

if (sw_plotall)
    plt_ctr = plt_ctr+1;
    figure(plt_ctr)
    plot(YC/mm,Corr1,YC/mm,Corr2,YC/mm,Corr3)
    xlabel('y [mm]')
    ylabel('s_j(y)')
    title('Signals from the three masks')
    legend('Mask1','Mask2','Mask3')
end


%% write a *.mat file

save MicrofluidicSPIFISimulations