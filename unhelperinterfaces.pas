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




implementation

end.
