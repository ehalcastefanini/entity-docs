<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRbankAccounts` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRbankAccounts` code unit is designed to manage and display bank account information in a structured form. It provides a user interface for viewing, editing, and interacting with bank account details such as Bank ID, Short Name, Account Name, Swift Code, Currency, Mill, Account Number, and Description. The main objective is to facilitate the management of bank account data in a user-friendly and efficient manner.

### Technologies Used:
- **Delphi**: The code is written in Delphi, utilizing its VCL (Visual Component Library) for UI components.
- **SOAP Services**: The code interacts with external services for data retrieval and updates.
- **Custom Components**: Includes custom components like `TFRAMEFindEditSOA` and `TFRAMEstatusInfo`.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - Labels (`TsLabel`): Display field names.
  - Text Fields (`TsDBEdit`): Editable fields for user input.
  - Custom Find Components (`TFRAMEFindEditSOA`): For searching and selecting related data.
- **Form Actions and Effects**:
  - Data entry and validation.
  - Interaction with external services for data retrieval and updates.

---

## 2. Functionality Description:

### User/Software Actions:
- Users can input or edit bank account details.
- The form interacts with external services to fetch or update data.
- Specific fields like Currency, Mill, and Seller are linked to search functionalities.

### Main Components:
- **Labels (`TsLabel`)**: Display field names.
- **Editable Fields (`TsDBEdit`)**: Allow users to input or edit data.
- **Custom Frames (`TFRAMEFindEditSOA`)**: Provide search and selection functionalities for related data.
- **Status Info Frame (`TFRAMEstatusInfo`)**: Displays the status of the current data.

### Pseudo-code for Actions and Events:
- `OnExit` event of `EDTshortName`: `if field loses focus then validate field value`.
- `OnSetAccessMode` event: `if access mode changes then update control states`.
- `OnCreate` event: `initialize form components and set default properties`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The `Create` constructor initializes the form, sets default properties, and configures components.
   - External services are linked for data operations.
2. **User Interaction**:
   - Users input data into fields or use search components to fetch related data.
   - Events like `OnExit` validate user input.
3. **Functions**:
   - `m_SetFindCurrency`: Configures the currency search component.
   - `m_SetFindMill`: Configures the mill search component.
   - `m_SetFindSeller`: Configures the seller search component.
   - `m_SetAccessMode`: Updates control states based on access mode.

### Required Data:
- Bank ID, Short Name, Account Name, Swift Code, Currency, Mill, Account Number, and Description.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Save Button**: Enabled only if all required fields are filled and valid.
- **Search Components**: Require user interaction to fetch related data.

### Available Filters:
- Currency, Mill, and Seller filters are available through search components.

### Error Messages:
- "Required field not completed" if a mandatory field is empty.
- "Invalid input" if a field value does not meet validation criteria.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Bank ID: Read-only.
- Short Name: Validated on exit.
- Currency, Mill, and Seller: Require valid selections.

---

## 5. Main Functions:

### Functions:
- `m_SetFindCurrency`: Configures the currency search component.
- `m_SetFindMill`: Configures the mill search component.
- `m_SetFindSeller`: Configures the seller search component.
- `m_SetAccessMode`: Updates control states based on access mode.

---

## 6. API Service Consumption:

### External Service Calls:
- **Service Name**: `TBankServiceUtils`.
- **Purpose**: Fetch and update bank account data.
- **Error Handling**: Displays error messages if the service call fails.

---

## 7. Conditional Fields (Form Logic):

- **Address Field**: Not applicable in this form.
- **Conditions**: Not explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **SOAP Services**: For data operations.
- **Custom Components**: `TFRAMEFindEditSOA`, `TFRAMEstatusInfo`.

### Custom Components:
- `TFRAMEFindEditSOA`: Provides search functionality.
- `TFRAMEstatusInfo`: Displays status information.

---

## 9. Fields and Validations Listing:

### Fields:
- **Bank ID**: (type: string, read-only).
- **Short Name**: (type: string, required).
- **Account Name**: (type: string, required).
- **Swift Code**: (type: string, optional).
- **Currency**: (type: string, required, linked to search component).
- **Mill**: (type: string, required, linked to search component).
- **Account Number**: (type: string, required).
- **Description**: (type: string, optional).

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
procedure TFRAMEbankAccounts.m_SetFindCurrency;
begin
  // Configure currency search component
end;
```

### Screenshots:
HTML representation of the form:
```html
<div style="width: 698px; height: 480px; border: 1px solid #ccc; padding: 10px;">
  <label style="display: block;">Bank ID:</label>
  <input type="text" readonly />
  <label style="display: block;">Bank Short Name:</label>
  <input type="text" />
  <label style="display: block;">Account Name:</label>
  <input type="text" />
  <label style="display: block;">Swift:</label>
  <input type="text" />
  <label style="display: block;">Currency:</label>
  <input type="text" />
  <label style="display: block;">Mill:</label>
  <input type="text" />
  <label style="display: block;">Account:</label>
  <input type="text" />
  <label style="display: block;">Description:</label>
  <input type="text" />
</div>
```

---

## 11. Important Comments in the Code:

- `// SET DAS PROPRIEDADES DA FRAME`: Highlights the initialization of frame properties.
- `// Finds`: Indicates the configuration of search components.

---

## 12. Conclusion:

The `FRbankAccounts` code unit provides a robust framework for managing bank account data. Its strengths lie in its modular design and integration with external services. However, the lack of explicit error handling and default values for fields could be improved.

---

## 13. Short Summary:

The `FRbankAccounts` form manages bank account data, providing fields for input and integration with external services for data retrieval and updates. It includes validation and search functionalities for related data like Currency, Mill, and Seller.#### **FRbankAccounts.pas**

```
unit FRbankAccounts;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, StdCtrls, Mask, DBCtrls,
  sDBEdit, kneFRStatusInfo, sLabel, sFrameAdapter, ImgList, ActnList,
  ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, Buttons, sBitBtn, sPanel,
  kneFRFindEditSOA;

type
  TFRAMEbankAccounts = class(TFRAMEBaseCtrlEditSOA)
    LBL1: TsLabel;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    EDTbankID: TsDBEdit;
    LBLshortName: TsLabel;
    EDTshortName: TsDBEdit;
    LBLbankName: TsLabel;
    EDTbankName: TsDBEdit;
    LBLswift: TsLabel;
    EDTswift: TsDBEdit;
    LBLcurrency: TsLabel;
    LBLmill: TsLabel;
    FRAMEfindCurrency: TFRAMEFindEditSOA;
    FRAMEfindMill: TFRAMEFindEditSOA;
    LBLcountNumber: TsLabel;
    EDTcountNumber: TsDBEdit;
    LBLdesc: TsLabel;
    EDTdescrip1: TsDBEdit;
    EDTdescrip2: TsDBEdit;
    EDTdescrip3: TsDBEdit;
    LBLseller: TsLabel;
    FRAMEfindSeller: TFRAMEFindEditSOA;
    procedure EDTshortNameExit(Sender: TObject);
  private
    procedure m_SetFindCurrency;
    procedure m_SetFindMill;
    procedure m_SetFindSeller;

    procedure m_AfterFindMill(Sender: TObject);
    procedure m_AfterFindSeller(Sender: TObject);
    procedure m_SetAccessMode(Sender: TObject; var pv_State: Boolean);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
  end;

var
  FRAMEbankAccounts: TFRAMEbankAccounts;

implementation

uses
  kneTypes, Global, kneUtils,
  //---
  BankServiceUtils, CurrencyServiceUtils, MillServiceUtils, SellerServiceUtils;

{$R *.dfm}

constructor TFRAMEbankAccounts.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'Bank';
  PropertyName := '';
  FrameType := frtMaster;  

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TBankServiceUtils.Create(self);

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;
  OnSetAccessMode := m_SetAccessMode;

  // Finds
  m_SetFindCurrency;
  m_SetFindMill;
  m_SetFindSeller;
end;


procedure TFRAMEbankAccounts.m_SetAccessMode(Sender: TObject; var pv_State: Boolean);
var b_enabled : Boolean;
begin

  TkneControls.SetControlState(EDTbankID, False);
end;

```

#### **FRbankAccounts.dfm**

```
inherited FRAMEbankAccounts: TFRAMEbankAccounts
  Width = 698
  Height = 480
  object LBL1: TsLabel [0]
    Left = 16
    Top = 14
    Width = 41
    Height = 13
    Caption = 'Bank ID:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLshortName: TsLabel [1]
    Left = 16
    Top = 158
    Width = 86
    Height = 13
    Caption = 'Bank Short Name:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLbankName: TsLabel [2]
    Left = 16
    Top = 186
    Width = 73
    Height = 13
    Caption = 'Account Name:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLswift: TsLabel [3]
    Left = 16
    Top = 130
    Width = 28
    Height = 13
    Caption = 'Swift:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLcurrency: TsLabel [4]
    Left = 16
    Top = 101
    Width = 48
    Height = 13
    Caption = 'Currency:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLmill: TsLabel [5]
    Left = 16
    Top = 41
    Width = 18
    Height = 13
    Caption = 'Mill:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLcountNumber: TsLabel [6]
    Left = 16
    Top = 214
    Width = 43
    Height = 13
    Caption = 'Account:'
    ParentFont = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LBLdesc: TsLabel [7]
    Left = 16
    Top = 242
    Width = 73
    Height = 13
    Caption = 'Account Name:'
```
<!-- tabs:end -->

