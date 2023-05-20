unit unHelperInterfaces;

{$mode ObjFPC}{$H+}

{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, Matrix, Graphics, types;

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


  { IBasicVector }

  IBasicVector = interface
    ['{19AA6C08-C142-4F77-B274-82822F9B823E}']
    function GetVector: Tvector2_double;
    function GetAngle: double;
    procedure SetAngle(AValue: double);
    function GetInitialVelocity: double;
    procedure SetInitialVelocity(const AValue: double);
    function GetOrigin: TPointF;
    procedure SetOrigin(const ptOrigin: TPointF);
    function GetEndTime: double;
    procedure SetEndTime(const dTime: double);
    function GetStartTime: double;
    function GetXAtTime(const dTime: double): double;
    function GetYAtTime(const dTime: double): double;
    function GetTimeToXDeplacement(const dDeplacement: double): double;
    function GetTimeToYDeplacement(const dDeplacement: double): double;
    function GetXAtStop: double;
    function GetYAtStop: double;
    function GetTimeToStop: double;
    procedure ReverseX();
    procedure ReverseY();
    function GetVelocityAtTime(const dTime: double): double;
    function GetVelocityVectorAtTime(const dTime: double): Tvector2_double;
    function ToString(): string; override;
    function Clone: IBasicVector;
    property InitialVelocity: double read GetInitialVelocity write SetInitialVelocity;
    property Origin: TPointF read GetOrigin write SetOrigin;
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
    {$IFDEF DEBUG}
    function GetLabel: string;
    procedure SetLabel(const sLabel: string);
    {$ENDIF}
    procedure SetPenColor(const clr: TColor);
    property Radius: double read GetRadius;
    property Mass: double read GetMass;
    property BrushColor: TColor read GetBrushColor write SetBrushColor;
    property PenColor: TColor read GetPenColor write SetPenColor;
    function ToString(): string;
    {$IFDEF DEBUG}
    property Text: string read GetLabel write SetLabel;
    {$ENDIF}
  end;

  IPathPart = interface
    ['{8AA65907-3C7B-4141-BCF2-8FFF3EC44204}']
    function GetCircle: ICircle;
    function GetVector: IBasicVector;
    function ToString: string;
    property Circle: ICircle read GetCircle;
    property Vector: IBasicVector read GetVector;
  end;

  IPathPartList = interface
    ['{E99CFD3E-400C-4098-8F57-4C0D0472E306}']
    function getItem(const iIndex: integer): IPathPart;
    function GetCount: cardinal;
    procedure Clear;
    procedure Add(const intfPathPart: IPathPart);
    procedure Delete(const intfPathPart: IPathPart);
    property Count: cardinal read GetCount;
    property Item[const iIndex: integer]: IPathPart read GetItem; default;
  end;

  IIdentity = interface
    ['{50E4CB90-4B85-4C51-9676-50A5F5A44F6F}']
    function GetId: cardinal;
    property Id: cardinal read GetId;
  end;

  ICirclesList = interface
    ['{E99CFD3E-400C-4098-8F57-4C0D0472E306}']
    function getItem(const iIndex: integer): ICircle;
    function GetCount: cardinal;
    procedure Clear;
    procedure Add(const intfCircle: ICircle);
    property Count: cardinal read GetCount;
    property Item[const iIndex: integer]: ICircle read GetItem; default;
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


  ITimeslice = interface
    ['{DFA80F2A-1162-416C-BFE2-DB184174C724}']
    function GetStartTime: double;
    function GetEndTime: double;
    function GetPathParts: IPathPartList;
    procedure SetStartTime(const dStartTime: double);
    procedure SetEndTime(const dEndTime: double);
    property StartTime: double read GetStartTime write SetStartTime;
    property EndTime: double read GetEndTime write SetEndTime;
    property PathParts: IPathPartList read GetPathParts;
    function ToString: string;
  end;

  ITimesliceList = interface
    ['{600F53EB-6DAD-41F1-865A-227196461EC4}']
    function getItem(const iIndex: integer): ITimeslice;
    function GetCount: cardinal;
    function indexOf(Const intfTimeSlice: ITimeslice): Integer;
    procedure Clear;
    procedure Add(const intfTimeslice: ITimeslice);
    property Count: cardinal read GetCount;
    property Item[const iIndex: integer]: ITimeslice read GetItem; default;
  end;


  IPathPlotter = interface
    ['{146574A6-3ED5-4123-84FD-B1810CF7C96C}']
    function GetTimeslices: ITimesliceList;
    procedure AddCircleWithPosition(const ACircle: ICircle; const Position: TPointF);
    procedure Clear;
    procedure GainThePlot;
    procedure Reinitialize;
    function GetThePlotAtTime(const dTime: double): ITimeslice;
    property Timeslices: ITimesliceList read GetTimeslices;
  end;



implementation

end.
