unit unOtherCircles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Fgl, unCirclePhysics, unHelperInterfaces;

type

  { TCircle }

  TCircle = class(TInterfacedObject, ICircle)
  private
    FdRadius: double;
    FdCenterX: double;
    FdCenterY: double;
    FclrBrush: TColor;
    FclrPen: TColor;
    FbStationary: boolean;
  protected
    function GetCenterX: double;
    procedure SetCenterX(const cX: double);
    function GetCenterY: double;
    procedure SetCenterY(const cY: double);
    function GetRadius: double;
    function GetBrushColor: TColor;
    procedure SetBrushColor(const clr: TColor);
    function GetPenColor: TColor;
    procedure SetPenColor(const clr: TColor);
    function GetStationary: boolean;
    procedure SetStationary(const bStationary: boolean);
  public
    constructor Create(const dCenterX, dCenterY, dRadius: double); virtual;
    function Distance(const dOtherCenterX, dOtherCenterY: double): double;
    procedure Render(const ACanvas: TCanvas);
  end;




implementation


{ TCircle }

function TCircle.GetCenterX: double;
begin
  Result := FdCenterX;
end;

procedure TCircle.SetCenterX(const cX: double);
begin
  FdCenterX := cX;
end;

function TCircle.GetCenterY: double;
begin
  Result := FdCenterY;
end;

procedure TCircle.SetCenterY(const cY: double);
begin
  FdCenterY := cY;
end;

function TCircle.GetRadius: double;
begin
  Result := FdRadius;
end;

function TCircle.GetBrushColor: TColor;
begin
  Result := FclrBrush;
end;

procedure TCircle.SetBrushColor(const clr: TColor);
begin
  FclrBrush := clr;
end;

function TCircle.GetPenColor: TColor;
begin
  Result := FclrPen;
end;

procedure TCircle.SetPenColor(const clr: TColor);
begin
  FclrPen := clr;
end;

function TCircle.GetStationary: boolean;
begin
  Result := FbStationary;
end;

procedure TCircle.SetStationary(const bStationary: boolean);
begin
  FbStationary := bStationary;
end;

constructor TCircle.Create(const dCenterX, dCenterY, dRadius: double);
begin
  FdCenterX := dCenterX;
  FdCenterY := dCenterY;
  FdRadius := dRadius;
  FclrBrush := clWhite;
  FclrPen := clBlack;
  FbStationary := True;
end;

function TCircle.Distance(const dOtherCenterX, dOtherCenterY: double): double;
begin
  Result := Sqrt(Sqr(dOtherCenterX - FdCenterX) + Sqr(dOtherCenterY - FdCenterY));
end;

procedure TCircle.Render(const ACanvas: TCanvas);
begin
  ACanvas.Brush.color := FclrBrush;
  ACanvas.Pen.Color := FclrPen;
  ACanvas.Ellipse(Round(FdCenterX - FdRadius), Round(FdCenterY - FdRadius),
    Round(FdCenterX + FdRadius), Round(FdCenterY + FdRadius));
end;

end.
