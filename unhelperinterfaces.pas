unit unHelperInterfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Matrix, Graphics;

type
  IBasicLogger = interface
    ['{2D3C8D9A-8E26-4B28-A66F-74F1BCCFFA54}']
    procedure LogMessage(const sMessage: string);
  end;

  IBasicLoggerClient = interface(IBasicLogger)
    ['{8ECB82CB-DD04-4E7B-8F3C-7188E320A28A}']
    procedure SetLogger(const intfLogger: IBasicLogger);
  end;

  I2DVector = interface ['{ED2CF05C-5AA5-4C3E-994C-36D0788EA64D}']
    function GetMagnitude: double;
    function GetAngle: double;
    function GetVector: Tvector2_double;
    function GetNormalised: I2DVector;
    function GetDotProduct(const AVector: I2DVector): double;
    function GetDotProductV(const AVec: Tvector2_double): double;
    function Minus(const AVector: I2DVector): I2DVector;
    property Magnitude: double read GetMagnitude;
    property Angle: double read GetAngle;
    property Vector: Tvector2_double read GetVector;
  end;

  I2DVectorFactory = interface ['{944D01C4-CF45-49C1-A316-1C321D355D77}']
    function CreateWithAngle(const dMagnitude, dAngle: double):I2DVector;
    function Create(const dXLength, dYLength: double):I2DVector;
  end;

  IGameCircle = interface
    ['{B8C03731-DE0F-4808-B4B8-8A7C9E2ACFE2}']
    function GetXAtTime(const dTime: double): double;
    function GetYAtTime(const dTime: double): double;
    function GetTimeToXDeplacement(const dDeplacement: double): double;
    function GetTimeToYDeplacement(const dDeplacement: double): double;
    function GetDisplacementXAtStop: double;
    function GetDisplacementYAtStop: double;
    function GetTimeToStop: double;
    function GetInitialVelocity: double;
    procedure SetInitialVelocity(const dVel: double);
    function GetEndTime: double;
    procedure SetEndTime(const dTime: double);
    function GetOriginX: double;
    procedure SetOriginX(const dX: double);
    function GetOriginY: double;
    procedure SetOriginY(const dY: double);
    function GetAngle: double;
    procedure SetAngle(const dAngle: double);
    function GetStartTime: double;
    function GetVector: TVector2_Double;
    function GetRadius: double;
    function GetFillColor: TColor;
    procedure SetFillColor(const clr: TColor);
    function GetLineColor: TColor;
    procedure SetLineColor(const clr: TColor);
    function GetStationary: boolean;
    procedure SetStationary(const bStationary: boolean);
    function GetDistance(const ACircle: IGameCircle): double;
    procedure ReverseX();
    procedure ReverseY();
    function GetVelocityVectorAtTime(const dTime: double): Tvector2_double;
    function ToString(): string; override;
    procedure Render(const ACanvas: TCanvas; const dTime: double);
    property InitialVelocity: double read GetInitialVelocity write SetInitialVelocity;
    property OriginX: double read GetOriginX write SetOriginX;
    property OriginY: double read GetOriginY write SetOriginY;
    property Angle: double read GetAngle write SetAngle;
    property EndTime: double read GetEndTime write SetEndTime;
    property StartTime: double read GetStartTime;
    property Vector: Tvector2_double read GetVector;
    property Radius: double read GetRadius;
    property FillColor: TColor read GetFillColor write SetFillColor;
    property LineColor: TColor read GetLineColor write SetLineColor;
    property Stationary: boolean read GetStationary write SetStationary;
  end;

  IBasicVector = interface
    ['{19AA6C08-C142-4F77-B274-82822F9B823E}']
    function GetVector: Tvector2_double;
    function GetAngle: double;
    procedure SetAngle(AValue: double);
    function GetInitialVelocity: double;
    procedure SetInitialVelocity(const AValue: double);
    function GetOriginX: double;
    function GetOriginY: double;
    procedure SetOriginX(const dX: double);
    procedure SetOriginY(const dY: double);
    function GetEndTime: double;
    procedure SetEndTime(const dTime: double);
    function GetStartTime: double;
    function GetXAtTime(const dTime: double): double;
    function GetYAtTime(const dTime: double): double;
    function GetTimeToXDeplacement(const dDeplacement: double): double;
    function GetTimeToYDeplacement(const dDeplacement: double): double;
    function GetDisplacementXAtStop: double;
    function GetDisplacementYAtStop: double;
    function GetTimeToStop: double;
    procedure ReverseX();
    procedure ReverseY();
    function GetVelocityVectorAtTime(const dTime: double): Tvector2_double;
    function ToString(): string; override;
    property InitialVelocity: double read GetInitialVelocity write SetInitialVelocity;
    property OriginX: double read GetOriginX write SetOriginX;
    property OriginY: double read GetOriginY write SetOriginY;
    property Angle: double read GetAngle write SetAngle;
    property EndTime: double read GetEndTime write SetEndTime;
    property StartTime: double read GetStartTime;
    property Vector: Tvector2_double read GetVector;
  end;

  ICircle = interface
    ['{2535367A-D8B6-4B38-B5AC-82576F719FF5}']
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
    property Radius: double read GetRadius;
    property CenterX: double read GetCenterX write SetCenterX;
    property CenterY: double read GetCenterY write SetCenterY;
    property BrushColor: TColor read GetBrushColor write SetBrushColor;
    property PenColor: TColor read GetPenColor write SetPenColor;
    property Stationary: boolean read GetStationary write SetStationary;
    function Distance(const dOtherCenterX, dOtherCenterY: double): double;
    procedure Render(const ACanvas: TCanvas);
  end;




implementation

end.
