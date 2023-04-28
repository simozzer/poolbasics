unit unPhysicsArea;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls;

type

  { TAngleControl }

  TAngleControl = class(TCustomControl)
  private
    FbmpBack: TBitmap;
    FcWidth: cardinal;
    FcHeight: cardinal;
    FclrFill: TColor;
    FclrCircleFill: TColor;
    FclrAngleColor: TColor;
    FdAngle: double;
    FOnChange: TNotifyEvent;
    FbSettingAngle: boolean;
    procedure SetAngleColor(AValue: TColor);
    procedure SetFillColor(AValue: TColor);
    procedure SetCircleFillColor(AValue: TColor);
  protected
    function GetWidth: cardinal;
    function GetHeight: cardinal;
    procedure SetWidth(const cWidth: cardinal);
    procedure SetHeight(const cHeight: cardinal);
    procedure SetAngle(const dAngle: double);
    function GetBackCanvas: TCanvas;
    procedure FillBackground;
    procedure DrawBackGround;
    procedure DrawAngleIndicator;
    procedure DrawCircle;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property Width: cardinal read GetWidth write SetWidth;
    property Height: cardinal read GetHeight write SetHeight;
    property BackCanvas: TCanvas read GetBackCanvas;
    property FillColor: TColor read FclrFill write SetFillColor;
    property AngleColor: TColor read FclrAngleColor write SetAngleColor;
    property Angle: double read FdAngle write SetAngle;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

  end;

implementation

uses
  Math;

const
  DEFAULT_WIDTH = 100;
  DEFAULT_HEIGHT = 100;




{ TAngleControl }

procedure TAngleControl.SetFillColor(AValue: TColor);
begin
  if FclrFill = AValue then Exit;
  FclrFill := AValue;
  Invalidate;
end;

procedure TAngleControl.SetAngleColor(AValue: TColor);
begin
  if FclrAngleColor = AValue then Exit;
  FclrAngleColor := AValue;
  Invalidate;
end;

procedure TAngleControl.SetCircleFillColor(AValue: TColor);
begin
  if (FclrCircleFill = AValue) then exit;
  FclrCircleFill := AValue;
  Invalidate;
end;


function TAngleControl.GetWidth: cardinal;
begin
  Result := FcWidth;
end;

function TAngleControl.GetHeight: cardinal;
begin
  Result := FcHeight;
end;

procedure TAngleControl.SetWidth(const cWidth: cardinal);
begin
  if cWidth = FcWidth then exit;
  inherited Width := cWidth;
  FcWidth := cWidth;
  FbmpBack.Width := cWidth;
  Invalidate;
end;

procedure TAngleControl.SetHeight(const cHeight: cardinal);
begin
  if cHeight = FcHeight then exit;
  inherited Height := cHeight;
  FbmpBack.Height := cHeight;
  FcHeight := cHeight;
  Invalidate;
end;

procedure TAngleControl.SetAngle(const dAngle: double);
var
  d: double;
begin
  d := dAngle;
  if (d = FdAngle) then Exit;
  if (d > 2 * pi) then
    d := d - (2 * PI);
  if (d < 0) then
    d := d + (2 * PI);
  FdAngle := d;
  invalidate();
  if assigned(FOnChange) then FOnChange(Self);
end;

function TAngleControl.GetBackCanvas: TCanvas;
begin
  Result := FbmpBack.Canvas;
end;

procedure TAngleControl.FillBackground;
begin
  BackCanvas.Pen.Color := FclrFill;
  BackCanvas.Brush.Color := FclrFill;
  BackCanvas.Rectangle(0, 0, FcWidth, FcHeight);
end;

procedure TAngleControl.DrawBackGround;
begin
  FillBackground;
  DrawCircle;
  DrawAngleIndicator;
end;

procedure TAngleControl.DrawAngleIndicator;
begin
  BackCanvas.MoveTo(FcWidth div 2, FcHeight div 2);
  BackCanvas.Pen.Color := FclrAngleColor;
  BackCanvas.LineTo(50 + Round(50 * cos(-FdAngle)), 50 + Round(50 * sin(FdAngle)));
  BackCanvas.Ellipse(45,45,55,55);
end;

procedure TAngleControl.DrawCircle;
begin
  BackCanvas.Brush.Color := FclrCircleFill;
  BackCanvas.Ellipse(0, 0, Width, Height);
end;

procedure TAngleControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
var
  dAngle: double;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    FbSettingAngle := True;
    dAngle := ArcTan2((Y - ClientHeight / 2) / ClientHeight,
      (X - ClientWidth / 2) / ClientWidth);
    SetAngle(dAngle);
  end;
end;

procedure TAngleControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
    FbSettingAngle := False;
end;

procedure TAngleControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  dAngle :Double;
begin
  inherited MouseMove(Shift, X, Y);
  if FbSettingAngle then
  begin
       dAngle := ArcTan2((Y - ClientHeight / 2) / ClientHeight,
      (X - ClientWidth / 2) / ClientWidth);
    SetAngle(dAngle);
  end;
end;

procedure TAngleControl.Paint;
begin
  inherited Paint;
  DrawBackGround;
  Canvas.Draw(0, 0, FbmpBack);

end;

constructor TAngleControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FbmpBack := TBitmap.Create;
  Width := DEFAULT_WIDTH;
  Height := DEFAULT_HEIGHT;
  FclrFill := clBtnFace;
  FclrCircleFill := clLtGray;
  FclrAngleColor := clWhite;
  FdAngle := 0;
  FOnChange := nil;
  FbSettingAngle := False;
  BorderStyle:=bsNone;
  BorderWidth:=0;
end;

end.
