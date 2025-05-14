<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustLists` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRcustLists` code unit defines a Delphi frame (`TFRAMEcustLists`) that provides a user interface for managing customer lists. It includes form elements for entering and displaying customer-related data such as market, reference, remarks, customer list code, and business unit. The frame integrates with data sources and services to handle data operations, ensuring a seamless user experience for managing customer lists.

### Technologies Used:
- **Delphi Framework**: For building the user interface and handling events.
- **SOAP Services**: For interacting with external services (`CustomerListServiceUtils` and `CustomerMarketServiceUtils`).
- **Database Components**: For binding UI elements to data sources.
- **Custom Components**: Includes `TFRAMEstatusInfo`, `TFRAMEFindEditSOA`, and `TFRAMEBusUnit`.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `EDTreference` (Text Input - Database-bound)
  - `EDTremarks` (Text Input - Database-bound)
  - `EDTcustListCd` (Text Input - Database-bound, read-only in certain conditions)
  - `FRAMEFindMarket` (Custom Component for market selection)
  - `FRAMEBusUnit1` (Custom Component for business unit selection)
- **Form Actions and Effects**:
  - Data initialization and binding to the frame.
  - Market selection triggers specific events.
  - Business unit selection is pre-configured with default values.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can input or view customer list details such as market, reference, remarks, and business unit.
- The frame initializes data and binds it to the UI components.
- Market selection triggers a custom event (`OnChangeMkt`).

### Main Components:
- **Labels (`TsLabel`)**: Display field names.
- **Database-bound Text Inputs (`TsDBEdit`)**: Allow users to input or view data.
- **Custom Components**:
  - `TFRAMEFindEditSOA`: Handles market selection.
  - `TFRAMEBusUnit`: Manages business unit selection and default values.
  - `TFRAMEstatusInfo`: Displays status information.

### Pseudo-code for Actions and Events:
- **OnClick Event of Buttons**: Not explicitly defined in the code.
- **OnChange Event of Market Field**:
  ```pseudo
  if market field value changes then
    trigger OnChangeMkt event
  ```
- **Initialization**:
  ```pseudo
  on frame creation:
    set data source and bind to UI components
    configure default values for business unit
    initialize data
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created, and its properties are set (e.g., `MasterSource`, `DataPacketName`).
   - Data sources are assigned to UI components.
   - Default values for the business unit are configured.
   - The `m_InitializeData` method is assigned to the `OnInitializeData` event.
2. **User Interaction**:
   - Users input data into the form fields.
   - Market selection triggers the `OnChangeMkt` event.
3. **Functions**:
   - `Create` (File: `FRcustLists.pas`): Initializes the frame and its components.
   - `SetKeyEditing` (File: `FRcustLists.pas`): Configures the state of the `EDTcustListCd` field.
   - `m_SetFindMarket` (File: `FRcustLists.pas`): Configures the market selection component.

### Required Data:
- Market, reference, remarks, customer list code, and business unit.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Market Selection**:
  - Preconditions: None explicitly defined.
  - Action: Triggers the `OnChangeMkt` event.
- **Customer List Code Field**:
  - Read-only when creating a new record.

### Available Filters:
- Market selection via `FRAMEFindMarket`.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Business Unit: Default value is set to `gv_DefaultBusUnit`.

### Field Validation and Conditions:
- `EDTcustListCd`: Read-only in certain conditions.
- Other field validations are not explicitly defined in the code.

---

## 5. Main Functions:

### Functions:
1. **`Create`**:
   - Initializes the frame and its components.
   - Configures data sources and default values.
2. **`SetKeyEditing`**:
   - Configures the state of the `EDTcustListCd` field.
3. **`m_SetFindMarket`**:
   - Configures the market selection component.

---

## 6. API Service Consumption:

### External Service Calls:
1. **Service Name**: `CustomerListServiceUtils`
   - **Endpoint**: Not explicitly defined.
   - **Purpose**: Provides data for customer lists.
2. **Service Name**: `CustomerMarketServiceUtils`
   - **Endpoint**: Not explicitly defined.
   - **Purpose**: Provides data for market selection.

---

## 7. Conditional Fields (Form Logic):

- **Market Field**:
  - Appears as part of the form and triggers the `OnChangeMkt` event when its value changes.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: For SOAP service communication.
- **DB and DBClient**: For database operations.

### Custom Components:
- `TFRAMEstatusInfo`
- `TFRAMEFindEditSOA`
- `TFRAMEBusUnit`

---

## 9. Fields and Validations Listing:

### Fields:
1. **Reference**:
   - Type: String
   - Required: Not explicitly defined.
2. **Remarks**:
   - Type: String
   - Required: Not explicitly defined.
3. **Customer List Code**:
   - Type: String
   - Read-only in certain conditions.
4. **Market**:
   - Type: Custom Component (`TFRAMEFindEditSOA`).
5. **Business Unit**:
   - Type: Custom Component (`TFRAMEBusUnit`).

### Mapping:
- Not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEcustLists.SetKeyEditing(const EditKey: Boolean);
begin
  inherited;
  TkneControls.SetControlState(EDTcustListCd, False);
  FMktValue := FRAMEFindMarket.Text;
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Initialization**:
  ```delphi
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'CustomerList';
  ```
- **Market Configuration**:
  ```delphi
  procedure TFRAMEcustLists.m_SetFindMarket;
  ```

---

## 12. Conclusion:

The `FRcustLists` code unit provides a robust framework for managing customer lists, integrating database-bound fields and custom components. While it effectively handles data initialization and user interaction, the lack of explicit error handling and field validation may limit its robustness.

---

## 13. Short Summary:

The `FRcustLists` unit defines a Delphi frame for managing customer lists, integrating database-bound fields, custom components, and SOAP services for seamless data handling and user interaction.#### **FRcustLists.pas**

```
unit FRcustLists;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, kneFRFindEditSOA, Mask, DBCtrls, sDBEdit, sLabel,
  kneFRStatusInfo, kneFRBusUnit;

type
  TFRAMEcustLists = class(TFRAMEBaseCtrlEditSOA)
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    LBL2: TsLabel;
    LBL3: TsLabel;
    LBLemail: TsLabel;
    FRAMEFindMarket: TFRAMEFindEditSOA;
    EDTreference: TsDBEdit;
    EDTremarks: TsDBEdit;
    LBL1: TsLabel;
    EDTcustListCd: TsDBEdit;
    LBLbusUnit: TsLabel;
    FRAMEBusUnit1: TFRAMEBusUnit;
  private
    FOnChangeMkt: TNotifyEvent;
    FMktValue: String;
    procedure m_SetFindMarket;
    procedure m_InitializeData(Sender: TDataSet);
    procedure SetOnChangeMkt(const Value: TNotifyEvent);
    procedure m_AfterSetFindMarket(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetKeyEditing(const EditKey: Boolean); override;

    property OnChangeMkt: TNotifyEvent read FOnChangeMkt write SetOnChangeMkt;
  end;

var
  FRAMEcustLists: TFRAMEcustLists;

implementation

uses
  kneTypes, kneUtils, kneFREditSOA, Global, 
  //---
  CustomerListServiceUtils, CustomerMarketServiceUtils;

{$R *.dfm}

{ TFRAMEcustLists }

constructor TFRAMEcustLists.Create(AOwner: TComponent);
begin
  inherited;
  
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'CustomerList';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TCustomerListServiceUtils.Create(self);

  m_SetFindMarket;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;

  FRAMEBusUnit1.DataSource     := DStable;
  FRAMEBusUnit1.BusUnitList    := gv_BusUnitList;
  FRAMEBusUnit1.BusUnitDefault := gv_DefaultBusUnit;

  OnInitializeData := m_InitializeData;

end;


procedure TFRAMEcustLists.SetKeyEditing(const EditKey: Boolean);
begin
  inherited;
  // impedir acesso ao campo (em NEW)
  TkneControls.SetControlState(EDTcustListCd, False);

  FMktValue := FRAMEFindMarket.Text;
end;


procedure TFRAMEcustLists.m_SetFindMarket;
begin
```

#### **FRcustLists.dfm**

```
inherited FRAMEcustLists: TFRAMEcustLists
  Font.Name = 'Verdana'
  object LBL2: TsLabel [0]
    Left = 8
    Top = 65
    Width = 44
    Height = 13
    Caption = 'Market:'
    FocusControl = FRAMEFindMarket.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBL3: TsLabel [1]
    Left = 8
    Top = 39
    Width = 63
    Height = 13
    Caption = 'Reference:'
    FocusControl = EDTreference
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLemail: TsLabel [2]
    Left = 8
    Top = 91
    Width = 56
    Height = 13
    Caption = 'Remarks:'
    FocusControl = EDTremarks
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBL1: TsLabel [3]
    Left = 8
    Top = 13
    Width = 74
    Height = 13
    Caption = 'Cust List Cd:'
    FocusControl = EDTcustListCd
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLbusUnit: TsLabel [4]
    Left = 243
    Top = 13
    Width = 81
    Height = 13
    Caption = 'Business Unit:'
    FocusControl = FRAMEBusUnit1.DBCBObusUnit
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  inherited PNLfooter: TsPanel
    Top = 215
    Width = 571
    TabOrder = 5
    Visible = False
  end
  inline FRAMEstatusInfo1: TFRAMEstatusInfo [6]
    Left = 0
    Top = 173
    Width = 571
    Height = 42
    Align = alBottom
    AutoScroll = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentBackground = False
    ParentFont = False
    TabOrder = 4
    inherited GRPstatus: TsGroupBox
      Width = 571
      Font.Name = 'Verdana'
      ParentFont = False
      inherited DBTXTlastUpd: TsDBText
        Width = 129
        Font.Color = 5059883
```
<!-- tabs:end -->

