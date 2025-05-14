<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcarrier` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRcarrier` code unit defines a form (`TFRAMEcarrier`) for managing carrier-related data. It provides a user interface for inputting, editing, and managing carrier details such as carrier code, name, country, language, currency, payment method, and remarks. The form integrates with backend services to fetch and save data, ensuring seamless interaction with the database.

### Technologies Used:
- **Delphi Framework**: Used for creating the form and managing components.
- **SOAP Services**: For backend communication via `SOAPHTTPClient`.
- **Database Components**: For data binding and interaction with the database.
- **Custom Components**: Includes `kneFRCtrlEditSOA`, `kneFRFindEditSOA`, `kneFRStatusInfo`, and others for specialized functionalities.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `EDTcode` (Text Input): Carrier Code.
  - `EDTabbrName` (Text Input): Abbreviated Name.
  - `EDTname` (Text Input): Carrier Name.
  - `EDTremarks1`, `EDTremarks2`, `EDTremarks3` (Text Input): Remarks fields.
  - `EDTlegalNum` (Text Input): Legal Number.
  - `FRAMEfindCountry`, `FRAMEfindLanguage`, `FRAMEfindCurrency`, `FRAMEfindPayment` (Custom Find Components): For selecting country, language, currency, and payment method.
- **Form Actions and Effects**:
  - Data initialization and validation.
  - Interaction with backend services for saving and retrieving data.
  - Dynamic updates based on user input.

---

## 2. Functionality Description:

### User/Software Actions:
- Input carrier details such as code, name, and remarks.
- Select associated country, language, currency, and payment method using custom find components.
- Save or update carrier data to the backend.

### Main Components:
- **Labels (`TsLabel`)**: Display field names and provide focus control.
- **Text Inputs (`TsDBEdit`)**: Allow users to input carrier details.
- **Custom Find Components (`TFRAMEFindEditSOA`)**: Enable selection of related entities like country and language.
- **Status Info (`TFRAMEstatusInfo`)**: Displays status information.
- **Backend Service (`TCarrierServiceUtils`)**: Handles data operations.

### Pseudo-code for Actions and Events:
- `OnChange` event of `EDTcode`:  
  `if field value changed then execute CodeChanged event`.
- `OnInitializeData` event:  
  `if form initialized then load data`.
- `AfterApplyChanges` event:  
  `if data changes applied then execute post-save logic`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created and initialized (`Create` constructor).
   - Backend service (`TCarrierServiceUtils`) is configured.
   - Custom find components are set up for country, language, payment, and currency.
   - Data initialization and post-save events are assigned.
2. **User Interaction**:
   - Users input data into text fields or select options using find components.
   - Changes trigger validation and backend service calls.
3. **Data Operations**:
   - Data is fetched or saved via `TCarrierServiceUtils`.

### Required User Data:
- Carrier Code, Name, Abbreviated Name, Legal Number, Remarks.
- Selected Country, Language, Currency, and Payment Method.

---

## 4. Business Rules:

### Actions and Preconditions:
- **CodeChanged Event**: Triggered when the carrier code changes.
- **Save Action**: Requires all mandatory fields to be filled.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Not explicitly defined in the code.

---

## 5. Main Functions:

- **`Create` Constructor**: Initializes the form, sets up services, and configures components.
- **`m_SetFindCountry`**: Configures the country selection component.
- **`m_SetFindLanguage`**: Configures the language selection component.
- **`m_SetFindPayment`**: Configures the payment method selection component.
- **`m_SetFindCurrency`**: Configures the currency selection component.
- **`m_InitializeData`**: Handles data initialization.
- **`m_AfterApplyChanges`**: Executes logic after data changes are applied.

---

## 6. API Service Consumption:

- **Service Name**: `TCarrierServiceUtils`.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Carrier details (e.g., code, name, country, etc.).
- **Data Received**: Confirmation of success or failure.
- **Purpose**: Fetch or save carrier data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: For SOAP-based backend communication.
- **DBClient**: For database interaction.

### Custom Components:
- **`TFRAMEFindEditSOA`**: For entity selection.
- **`TFRAMEstatusInfo`**: For displaying status information.
- **`TCarrierServiceUtils`**: For carrier-related backend operations.

---

## 9. Fields and Validations Listing:

- **Carrier Code (`EDTcode`)**: Type: string, required. Validation not defined.
- **Abbreviated Name (`EDTabbrName`)**: Type: string, optional. Validation not defined.
- **Name (`EDTname`)**: Type: string, required. Validation not defined.
- **Remarks (`EDTremarks1`, `EDTremarks2`, `EDTremarks3`)**: Type: string, optional. Validation not defined.
- **Legal Number (`EDTlegalNum`)**: Type: string, optional. Validation not defined.
- **Country, Language, Currency, Payment**: Selected via `TFRAMEFindEditSOA`.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```delphi
procedure TFRAMEcarrier.EDTcodeChange(Sender: TObject);
begin
  if Assigned(FCodeChanged) then
    FCodeChanged(Sender);
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Initialization**: The `Create` constructor contains critical setup logic for the form and its components.
- **Custom Find Components**: Methods like `m_SetFindCountry` configure essential components for selecting related entities.

---

## 12. Conclusion:

The `FRcarrier` code unit provides a robust form for managing carrier data, integrating with backend services for seamless data operations. While it offers a well-structured interface and functionality, the lack of explicit error handling, field validation, and default values limits its robustness.

---

## 13. Short Summary:

The `FRcarrier` code unit defines a form for managing carrier data, integrating with backend services for data operations. It includes fields for carrier details and custom components for selecting related entities like country and language.#### **FRcarrier.pas**

```
unit FRcarrier;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRFindEditSOA, kneFRStatusInfo, kneFRGridEditSOA, sDBEdit, sLabel,
  sFrameAdapter, sBitBtn, sPanel;

type
  TFRAMEcarrier = class(TFRAMEBaseCtrlEditSOA)
    FRAMEfindCountry: TFRAMEFindEditSOA;
    FRAMEfindPayment: TFRAMEFindEditSOA;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    FRAMEfindLanguage: TFRAMEFindEditSOA;
    FRAMEfindCurrency: TFRAMEFindEditSOA;
    LBLcarrierCode: TsLabel;
    Label1: TsLabel;
    Label3: TsLabel;
    Label4: TsLabel;
    Label5: TsLabel;
    Label6: TsLabel;
    Label7: TsLabel;
    Label8: TsLabel;
    Label2: TsLabel;
    EDTcode: TsDBEdit;
    EDTabbrName: TsDBEdit;
    EDTname: TsDBEdit;
    EDTremarks1: TsDBEdit;
    EDTremarks2: TsDBEdit;
    EDTremarks3: TsDBEdit;
    EDTlegalNum: TsDBEdit;
    procedure EDTcodeChange(Sender: TObject);
  private
    { Private declarations }
    mv_KeyInitVal: string;
    FCodeChanged: TNotifyEvent;
    procedure m_SetFindCountry;
    procedure m_SetFindLanguage;
    procedure m_SetFindPayment;
    procedure m_SetFindCurrency;
    procedure m_InitializeData(Sender: TDataSet);
    procedure m_AfterApplyChanges(Sender: TObject);
    procedure SetCodeChanged(const Value: TNotifyEvent);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    property CodeChanged: TNotifyEvent read FCodeChanged write SetCodeChanged;

  end;

var
  FRAMEcarrier: TFRAMEcarrier;

implementation

uses
  kneInterfaces, kneFindDialogSOA, kneUtils, kneTypes,
  CarrierServiceUtils, CountryServiceUtils, LanguageServiceUtils, 
  CurrencyServiceUtils, PaymentCarrServiceUtils, MaddressAndContact;

{$R *.dfm}

{ TFRAMEcarrier }

constructor TFRAMEcarrier.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';          // n�o necessita de estar def. na frame Master
  DataPacketName := 'Carrier';
  PropertyName := '';             // n�o necessita de estar def. na frame Master
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TCarrierServiceUtils.Create(self);

  //Configura��o do findEdit para o Consignee, Warehouse e Country
  m_SetFindCountry;
  m_SetFindLanguage;
  m_SetFindPayment;
  m_SetFindCurrency;

  // Atribui��o do evento de inicializa��o dos dados
  OnInitializeData := m_InitializeData;
  // Atribui��o do evento do After Apply
  AfterApplyChanges := m_AfterApplyChanges;

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
```

#### **FRcarrier.dfm**

```
inherited FRAMEcarrier: TFRAMEcarrier
  object LBLcarrierCode: TsLabel [0]
    Left = 8
    Top = 13
    Width = 53
    Height = 13
    Caption = 'Ca&rrier Cd:'
    FocusControl = EDTcode
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label1: TsLabel [1]
    Left = 8
    Top = 39
    Width = 31
    Height = 13
    Caption = '&Name:'
    FocusControl = EDTname
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label3: TsLabel [2]
    Left = 412
    Top = 65
    Width = 51
    Height = 13
    Caption = 'Lan&guage:'
    FocusControl = FRAMEfindLanguage.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label4: TsLabel [3]
    Left = 8
    Top = 65
    Width = 43
    Height = 13
    Caption = 'Countr&y:'
    FocusControl = FRAMEfindCountry.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label5: TsLabel [4]
    Left = 412
    Top = 91
    Width = 48
    Height = 13
    Caption = 'C&urrency:'
    FocusControl = FRAMEfindCurrency.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label6: TsLabel [5]
    Left = 8
    Top = 91
    Width = 46
    Height = 13
    Caption = 'Pay&ment:'
    FocusControl = FRAMEfindPayment.DBE
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label7: TsLabel [6]
    Left = 8
    Top = 117
    Width = 45
    Height = 13
    Caption = 'Remar&ks:'
    FocusControl = EDTremarks1
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object Label8: TsLabel [7]
```
<!-- tabs:end -->

