unit unOtherCircles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Fgl, matrix;

type

  IDeceleratingCircle = interface['{2AC3E6C6-87ED-4141-82D7-5DDDCB06E703}']
    function GetCenterX: Double;
    function GetCenterY: Double;
    function GetAngle: Double;
    function GetRadius: Double;
    function GetMass: Double;
    function GetInitialVelocity: Double;
    function GetTimeToStop: Double;
    function GetDistanceToStop: Double;
    function GetXDisplacementAtTime(const dTime:Double): Double;
    function GetYDisplacementAtTime(const dTime:Double): Double;
    function GetXVelocityAtTime(const dTime: DOuble): Double;
    function GetYVelocityAtTime(const dTime : Double): Double;
    function GetXDisplacementAtStop: Double;
    function GetYDisplacementAtStop: Double;
    function GetInitialMovementVector: Tvector2_double;
    function GetMovementVectorAtTime(const dTime :Double): Tvector2_double;

  end;

  { TCircle }

  TCircle = class
  private
    FdRadius: double;
    FdCenterX: double;
    FdCenterY: double;
    FclrBrush: TColor;
    FclrPen: TColor;
    FbStationary: Boolean;
  public
    constructor Create(const dCenterX, dCenterY, dRadius: double);
    property Radius: double read FdRadius;
    property CenterX: double read FdCenterX write FdCenterX;
    property CenterY: double read FdCenterY write FdCenterY;
    property BrushColor: TColor read FclrBrush write FclrBrush;
    property PenColor: TColor read FclrPen write FclrPen;
    property Stationary : Boolean read FbStationary write FbStationary;
    function Distance(const dOtherCenterX, dOtherCenterY: double): double;
    procedure Render(const ACanvas: TCanvas);
  end;

  TCirclesList = specialize TFPGObjectList<TCircle>;

implementation

{ TCircle }

constructor TCircle.Create(const dCenterX, dCenterY, dRadius: double);
begin
  FdCenterX := dCenterX;
  FdCenterY := dCenterY;
  FdRadius := dRadius;
  FclrBrush := clWhite;
  FclrPen := clBlack;
  FbStationary:= True;
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
  Round(FdCenterX +
    FdRadius), Round(FdCenterY + FdRadius));
end;

end.
