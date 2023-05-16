unit unHelperInterfaces;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Matrix, Graphics, FGL, types;

type
  IBasicLogger = interface
    ['{2D3C8D9A-8E26-4B28-A66F-74F1BCCFFA54}']
    procedure LogMessage(const sMessage: string);
  end;

  IBasicLoggerClient = interface(IBasicLogger)
    ['{8ECB82CB-DD04-4E7B-8F3C-7188E320A28A}']
    procedure SetLogger(const intfLogger: IBasicLogger);
  end;

  I2DVector = interface
    ['{ED2CF05C-5AA5-4C3E-994C-36D0788EA64D}']
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

  I2DVectorFactory = interface
    ['{944D01C4-CF45-49C1-A316-1C321D355D77}']
    function CreateWithAngle(const dMagnitude, dAngle: double): I2DVector;
    function Create(const dXLength, dYLength: double): I2DVector;
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
    function ToString(): string;
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

  { IBasicVector }

  IBasicVector = interface
    ['{19AA6C08-C142-4F77-B274-82822F9B823E}']
    function GetVector: Tvector2_double;
    function GetAngle: double;
    procedure SetAngle(AValue: double);
    function GetInitialVelocity: double;
    procedure SetInitialVelocity(const AValue: double);
    function GetOrigin : TPointF;
    procedure SetOrigin(Const ptOrigin:TPointF);
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
    function GetVelocityAtTime(const dTime: double): double;
    function GetVelocityVectorAtTime(const dTime: double): Tvector2_double;
    function ToString(): string; override;
    function Clone : IBasicVector;
    property InitialVelocity: double read GetInitialVelocity write SetInitialVelocity;
    property Origin : TPointF read GetOrigin write SetOrigin;
    property Angle: double read GetAngle write SetAngle;
    property EndTime: double read GetEndTime write SetEndTime;
    property StartTime: double read GetStartTime;
    property Vector: Tvector2_double read GetVector;
  end;

  { ICircle }

  ICircle = interface
    ['{2535367A-D8B6-4B38-B5AC-82576F719FF5}']
    function GetRadius: double;
    function GetMass: double;
    function GetBrushColor: TColor;
    procedure SetBrushColor(const clr: TColor);
    function GetPenColor: TColor;
    procedure SetPenColor(const clr: TColor);
    function GetStationary: boolean;
    procedure SetStationary(const bStationary: boolean);
    property Radius: double read GetRadius;
    property Mass: double read GetMass;
    property BrushColor: TColor read GetBrushColor write SetBrushColor;
    property PenColor: TColor read GetPenColor write SetPenColor;
    property Stationary: boolean read GetStationary write SetStationary;
    function ToString(): String;
  end;


  IObjectWithVector = interface
    ['{DE55D489-919F-42D2-B121-BA6955147E96}']
    function GetBasicVector: IBasicVector;
    property Vector: IBasicVector read GetBasicVector;
    function Clone: IUnknown;
  end;

  IPathPart = interface['{8AA65907-3C7B-4141-BCF2-8FFF3EC44204}']
    function GetCircle : ICircle;
    function GetVector : IBasicVector;
    function ToString: String;
    property Circle : ICircle read GetCircle;
    property Vector : IBasicVector read GetVector;
  end;

  IPathPartList = interface['{E99CFD3E-400C-4098-8F57-4C0D0472E306}']
    function getItem(const iIndex: Integer):IPathPart;
    function GetCount:Cardinal;
    procedure Clear;
    procedure Add(const intfPathPart : IPathPart);
    property Count: Cardinal read GetCount;
    property Item[const iIndex: Integer]: IPathPart read GetItem; default;
  end;

  IIdentity = interface ['{50E4CB90-4B85-4C51-9676-50A5F5A44F6F}']
    function GetId: Cardinal;
    property Id : Cardinal read GetId;
  end;

  ICirclesList = interface['{E99CFD3E-400C-4098-8F57-4C0D0472E306}']
    function getItem(const iIndex: Integer):ICircle;
    function GetCount:Cardinal;
    procedure Clear;
    procedure Add(const intfCircle : ICircle);
    property Count: Cardinal read GetCount;
    property Item[const iIndex: Integer]: ICircle read GetItem; default;
  end;

  ICircleCollisionResult = interface
    ['{5427548A-15C0-4A9C-B0C5-8E646F02DD2A}']
    function GetCircleID1: integer;
    function GetCircleID2: integer;
    function GetHitTime: double;
    function GetCircle1XAtHit: double;
    function GetCircle1YAtHit: double;
    function GetCircle2XAtHit: double;
    function GetCircle2YAtHit: double;
    property CircleId1: integer read GetCircleID1;
    property CircleId2: integer read GetCircleID2;
    property Circle1XAtHit: double read GetCircle1XAtHit;
    property Circle1YAtHit: double read GetCircle1YAtHit;
    property Circle2XAtHit: double read GetCircle2XAtHit;
    property Circle2YAtHit: double read GetCircle2YAtHit;
    property HitTime: double read GetHitTime;
  end;

  // Path information for 1 object, which can be queried by time
  ITrajectoryPaths = interface
    ['{98B877F0-13F2-4B85-AFFE-4E395428FF99}']
    function GetCount: cardinal;
    function GetItems: TInterfaceList;
    function getItem(const iIndex: cardinal): IBasicVector;
    function GetCircles: ICirclesList;
    procedure SetCircles(const lstCircles: ICirclesList);
    property Items: TInterfaceList read GetItems;
    property Item[const iIndex: cardinal]: IBasicVector read GetItem;
    property Count: cardinal read GetCount;
    property OtherCircles: ICirclesList read GetCircles write SetCircles;
    function GetXAtTime(const dTime: double): double;
    function GetYAtTime(const dTime: double): double;
    function GetVectorForTime(const dTime: double): IBasicVector;
    procedure CalculateTrajectories;
  end;

  ITimeslice = interface['{DFA80F2A-1162-416C-BFE2-DB184174C724}']
    function GetStartTime: Double;
    function GetEndTime: Double;
    function GetPathParts : IPathPartList;
    procedure SetStartTime(const dStartTime : Double);
    procedure SetEndTime(const dEndTime: Double);
    property StartTime : Double read GetStartTime write SetStartTime;
    property EndTime : Double read GetEndTime write SetEndTime;
    property PathParts : IPathPartList read GetPathParts;
    function ToString: String;
  end;

  ITimesliceList = interface['{600F53EB-6DAD-41F1-865A-227196461EC4}']
    function getItem(const iIndex: Integer):ITimeslice;
    function GetCount:Cardinal;
    procedure Clear;
    procedure Add(const intfTimeslice : ITimeslice);
    property Count: Cardinal read GetCount;
    property Item[const iIndex: Integer]: ITimeslice read GetItem; default;
  end;


  IPathPlotter = interface['{146574A6-3ED5-4123-84FD-B1810CF7C96C}']
    function GetTimeslices : ITimesliceList;
    procedure AddCircle(const ACircle: ICircle);
    procedure Clear;
    procedure GainThePlot;
    function GetThePlotAtTime(const dTime: Double) : ICirclesList;
    property Timeslices: ITimesliceList read GetTimeslices;
  end;



implementation

end.
