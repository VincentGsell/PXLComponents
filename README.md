# PXLComponents
Wrapper component for PXL graphics library

# Introduction

PXL components aims to be a Wrapper for the PXL library, create by Yuriy Kotsarenko.
The goal is to put a "relative" easy way to develop in a RAD manner.

Detail on PXL library be find here : 
http://www.asphyre.net/products/pxl

Source is available currently on SourceForge 
https://svn.code.sf.net/p/asphyre/code/

## Feature
- PXL Lib as magic under the wood.
- PXLEngine support currently fellowing renderer : OpenGL, DX9, DX11, SoftwareRasterizer
- PXLEngine support variety of Pixel format (A8R8G8B8, A8, ...)
- PXLSurface give access to raw DIB independant Bitmap
- Capability to manage Assets inside IDE, inside final EXE.
- Capability to manage with zero (0) code complexe scene layout.
- Delphi IDE integration

## RoadMap : 

This lib should be, on term , available for all PXL platform, starting compiler FPC 3.0+ DCC18+ (And Lazarus1.4+ and DelphiXE7+ IDE)
- fix and strongify existing code on delphi version.
- Adapt for lazarus
- Cross compilation ready for raspberry.
- Add tools.

## Note about Compilation :

- Dependancy : [PXL lib](https://svn.code.sf.net/p/asphyre/code/)
- Compile and install the package in standart way. Do not forget to add path of the compiled unit in your delphi IDE (Tools/options)
- In order to obtain a fully functional Software Rasterizer engine, please activate conditional define "SRT_RENDER_TO_GDI"
(located in PCL.Devices.SRT.pas, line 20)
- Without that, all is functionnal, but not the RasterEngine. Please note that the device rendering part of the rasterengine is available only on windows.

## ScreenShots : Delphi 10.2 IDE

PXLEngine main component
  ![Alt text](/../master/Ressources/PXLEngineComp.png?raw=true "PXLEngine component in all its glory")

PXLEngine Device Option
  ![Alt text](/../master/Ressources/PXLEngineDeviceOptionComp.png?raw=true "PXLEngine component in all its glory")

PXLSurface component overview, with its layout presentation
  ![Alt text](/../master/Ressources/PXLSurfaceComp.png?raw=true "PXLEngine component in all its glory")
  
