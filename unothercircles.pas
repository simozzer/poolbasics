unit unOtherCircles;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Fgl, unCirclePhysics;

type

  { TCircle }

  TCircle = class
  private
    FdRadius: double;
    FdCenterX: double;
    FdCenterY: double;
    FclrBrush: TColor;
    FclrPen: TColor;
    FbStationary: boolean;
  public
    constructor Create(const dCenterX, dCenterY, dRadius: double); virtual;
    property Radius: double read FdRadius;
    property CenterX: double read FdCenterX write FdCenterX;
    property CenterY: double read FdCenterY write FdCenterY;
    property BrushColor: TColor read FclrBrush write FclrBrush;
    property PenColor: TColor read FclrPen write FclrPen;
    property Stationary: boolean read FbStationary write FbStationary;
    function Distance(const dOtherCenterX, dOtherCenterY: double): double;
    procedure Render(const ACanvas: TCanvas);
  end;

  { TMovingCircle }

  TMovingCircle = class(TCircle)
  private
    FVector: T2DVector;
  public
    property Vector: T2DVector read FVector;
    constructor Create(const dCenterX, dCenterY, dRadius: double); override;
    destructor Destroy; override;

  end;

  TCirclesList = specialize TFPGObjectList<TCircle>;

implementation

{ TMovingCircle }

constructor TMovingCircle.Create(const dCenterX, dCenterY, dRadius: double);
begin
  inherited Create(dCenterX, dCenterY, dRadius);
  FVector := T2DVector.Create(0, 0);
end;

destructor TMovingCircle.Destroy;
begin
  FVector.Free;
  inherited Destroy;
end;

{ TCircle }

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
