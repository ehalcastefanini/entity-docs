<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRpaymentCode`

## 1. Overview:

### Objective:
The `FRpaymentCode` unit defines a form (`TFRAMEpaymentCode`) that facilitates the management of payment codes and related details such as discounts, due dates, and invoice dispositions. It provides a user interface for interacting with payment-related data, including input fields, checkboxes, and linked data sources.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the form and its components.
- **SOAP Services**: Utilized for interacting with external services (`TPaymentServiceUtils`).
- **Database Components**: Includes `TsDBEdit`, `TsDBCheckBox`, and `DataSource` for binding UI elements to database fields.

### Form Type:
This is a **form** with the following elements:
- **Form Elements and Types**:
  - `EDTpaymentCode`: Text input for payment code (type: `TsDBEdit`).
  - `EDTdiscount`: Text input for discount percentage (type: `TsDBEdit`).
  - `EDTdueDate`: Text input for due days (type: `TsDBEdit`).
  - `EDTdiscDate`: Text input for days of discount (type: `TsDBEdit`).
  - `CHKdscfinOnDocs`: Checkbox for discount finalization on documents (type: `TsDBCheckBox`).
  - `CHKchkDispCred`: Checkbox for credit disposition check (type: `TsDBCheckBox`).
  - `CHKgiroPayment`: Checkbox for giro payment (type: `TsDBCheckBox`).
  - `FRAMEfindInvDisp`: Find edit for invoice disposition (type: `TFRAMEFindEditSOA`).
  - `FRAMEFindordChklstdocs`: Find edit for order checklist documents (type: `TFRAMEFindEditSOA`).
- **Form Actions**:
  - `OnExit` event for `EDTpaymentCode`: Validates the payment code when the user exits the field.
  - `OnBeforeApplyChanges`: Custom event triggered before applying changes.

---

## 2. Functionality Description:

### User/Software Actions:
- Input payment code, discount percentage, due days, and days of discount.
- Select options using checkboxes for specific payment-related configurations.
- Use find dialogs to search and select invoice dispositions and order checklist documents.

### Main Components:
- **Input Fields**: Allow users to enter payment-related data.
- **Checkboxes**: Enable or disable specific payment options.
- **Find Dialogs**: Provide search functionality for related data.
- **Status Info Frame**: Displays status information about the current payment.

### Pseudo-code for Actions and Events:
- `OnExit` event of `EDTpaymentCode`:  
  `if user exits payment code field then validate payment code`.
- `OnBeforeApplyChanges` event:  
  `if changes are about to be applied then execute custom logic`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is initialized with default properties and configurations.
   - Data sources and services are set up (`TPaymentServiceUtils`).
   - Find dialogs are configured for invoice disposition and order checklist documents.
2. **User Interaction**:
   - Users input data into fields and interact with checkboxes.
   - Events like `OnExit` and `OnBeforeApplyChanges` are triggered based on user actions.

### Functions:
- `Create` (in `FRpaymentCode`): Initializes the form and its components.
- `m_SetFindInvDisp` (in `FRpaymentCode`): Configures the find dialog for invoice disposition.
- `m_SetFindOrdChklstdocs` (in `FRpaymentCode`): Configures the find dialog for order checklist documents.

### Required Data:
- Payment code, discount percentage, due days, days of discount, and other related configurations.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Payment Code Validation**: Triggered when the user exits the `EDTpaymentCode` field.
- **Apply Changes**: Triggered by the `OnBeforeApplyChanges` event.

### Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- `EDTpaymentCode`: Uppercase text input.
- `EDTdiscount`, `EDTdueDate`, `EDTdiscDate`: Numeric input expected.

---

## 5. Main Functions:

- **`Create`**: Initializes the form, sets up data sources, and configures find dialogs.
- **`m_SetFindInvDisp`**: Configures the find dialog for invoice disposition.
- **`m_SetFindOrdChklstdocs`**: Configures the find dialog for order checklist documents.

---

## 6. API Service Consumption:

- **Service Name**: `TPaymentServiceUtils`.
- **Purpose**: Provides payment-related services and data handling.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **Invoice Disposition Field**: Appears only when the user interacts with the `FRAMEfindInvDisp` component.
- **Conditions**: Not explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: For SOAP service communication.
- **DBClient**: For database operations.

### Custom Components:
- **`TFRAMEFindEditSOA`**: Custom component for find dialogs.
- **`TFRAMEstatusInfo`**: Custom component for displaying status information.

---

## 9. Fields and Validations Listing:

- **EDTpaymentCode**: (type: string, required, uppercase).
- **EDTdiscount**: (type: numeric, optional).
- **EDTdueDate**: (type: numeric, optional).
- **EDTdiscDate**: (type: numeric, optional).
- **CHKdscfinOnDocs**: (type: boolean, optional).
- **CHKchkDispCred**: (type: boolean, optional).
- **CHKgiroPayment**: (type: boolean, optional).

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMEpaymentCode.EDTpaymentCodeExit(Sender: TObject);
begin
  // Validate payment code logic here
end;
```

### Screenshots:
The DFM file represents a form with labeled input fields and checkboxes. Below is the HTML representation:

```html
<div style="width: 663px; font-family: Verdana;">
  <label for="EDTpaymentCode">Code:</label>
  <input id="EDTpaymentCode" type="text" style="text-transform: uppercase;" />
  <label for="EDTdiscount">Discount:</label>
  <input id="EDTdiscount" type="number" />
  <label for="EDTdueDate">Due Days:</label>
  <input id="EDTdueDate" type="number" />
  <label for="EDTdiscDate">Days of Discount:</label>
  <input id="EDTdiscDate" type="number" />
  <label for="CHKdscfinOnDocs">Discount Finalization on Documents:</label>
  <input id="CHKdscfinOnDocs" type="checkbox" />
</div>
```

---

## 11. Important Comments in the Code:

- **Initialization**: The `Create` method sets up the form and its components.
- **Find Dialog Configuration**: The `m_SetFindInvDisp` and `m_SetFindOrdChklstdocs` methods configure the find dialogs.

---

## 12. Conclusion:

The `FRpaymentCode` unit provides a robust form for managing payment codes and related configurations. While it includes essential functionalities like input validation and find dialogs, error handling and field validation logic are not explicitly defined, which could be a limitation.

---

## 13. Short Summary:

The `FRpaymentCode` unit defines a form for managing payment codes, discounts, and related configurations. It integrates database-bound fields, checkboxes, and find dialogs, leveraging SOAP services for data handling.#### **FRpaymentCode.pas**

```
unit FRpaymentCode;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, sFrameAdapter, ImgList,
  ActnList, ExtCtrls, Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons,
  sBitBtn, sPanel, Mask, DBCtrls, sDBEdit, sLabel, kneFRStatusInfo,
  kneFRFindEditSOA, sCheckBox, sDBCheckBox;

type
  TFRAMEpaymentCode = class(TFRAMEBaseCtrlEditSOA)
    LBL1: TsLabel;
    EDTpaymentCode: TsDBEdit;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    sLabel2: TsLabel;
    EDTdiscount: TsDBEdit;
    sLabel3: TsLabel;
    EDTdueDate: TsDBEdit;
    sLabel4: TsLabel;
    sLabel5: TsLabel;
    EDTdiscDate: TsDBEdit;
    sLabel1: TsLabel;
    FRAMEfindInvDisp: TFRAMEFindEditSOA;
    CHKdscfinOnDocs: TsDBCheckBox;
    CHKchkDispCred: TsDBCheckBox;
    FRAMEFindordChklstdocs: TFRAMEFindEditSOA;
    sLabel6: TsLabel;
    CHKgiroPayment: TsDBCheckBox;
    procedure EDTpaymentCodeExit(Sender: TObject);
  private
    FOnBeforeApplyChanges: TNotifyEvent;
    { Private declarations }
    procedure m_SetFindInvDisp;
    procedure SetOnBeforeApplyChanges(const Value: TNotifyEvent);
    procedure m_SetFindOrdChklstdocs;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;

    property OnBeforeApplyChanges: TNotifyEvent read FOnBeforeApplyChanges write SetOnBeforeApplyChanges;

  end;

var
  FRAMEpaymentCode: TFRAMEpaymentCode;

implementation

uses
  kneUtils {#18555},
  PaymentServiceUtils, kneTypes, InvoiceDispServiceUtils, kneFREditSOA,
  DocCheckListServiceUtils {#18555};

{$R *.dfm}

{ TFRAMEpaymentCode }

constructor TFRAMEpaymentCode.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterSource := nil;
  MasterKeyFields := '';
  DataPacketName := 'Payment';
  PropertyName := '';
  FrameType := frtMaster;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';

  // Chamada dos Finds
  m_SetFindInvDisp;
  m_SetFindOrdChklstdocs; //2014/02/12, #18555

  // SET DAS PROPRIEDADES DE SERVI�O E GRELHA
  ProviderService := TPaymentServiceUtils.Create(self);

  // Atribui��o da propriedade Datasource � frame, para atribuir aos v�rios controlos
  FRAMEstatusInfo1.DataSource := DStable;

end;

procedure TFRAMEpaymentCode.m_SetFindInvDisp;
begin
  with FRAMEfindInvDisp do
  begin
    // configura�ao da Find Edit
    EditSettings.DataSource := DStable;
    EditSettings.FieldNameForCode := 'FinvDisp';
    EditSettings.FieldNameForDesc := 'FInvDispDesc';

    // configura��o do Find Dialog
    FindDialog.Options.DataSelection.FieldNameForCode := 'invDispCode';
    FindDialog.Options.DataSelection.FieldNamesForDesc.Clear;
```

#### **FRpaymentCode.dfm**

```
inherited FRAMEpaymentCode: TFRAMEpaymentCode
  Width = 663
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBL1: TsLabel [0]
    Left = 8
    Top = 13
    Width = 35
    Height = 13
    Caption = 'C&ode:'
    FocusControl = EDTpaymentCode
  end
  object sLabel2: TsLabel [1]
    Left = 8
    Top = 39
    Width = 54
    Height = 13
    Caption = 'D&iscount:'
    FocusControl = EDTdiscount
  end
  object sLabel3: TsLabel [2]
    Left = 173
    Top = 39
    Width = 61
    Height = 13
    Caption = 'Due Days:'
    FocusControl = EDTdueDate
  end
  object sLabel4: TsLabel [3]
    Left = 8
    Top = 143
    Width = 53
    Height = 13
    Caption = 'Inv Disp:'
  end
  object sLabel5: TsLabel [4]
    Left = 323
    Top = 39
    Width = 100
    Height = 13
    Caption = 'Da&ys of discount:'
    FocusControl = EDTdiscDate
  end
  object sLabel1: TsLabel [5]
    Left = 116
    Top = 39
    Width = 22
    Height = 13
    Caption = '(%)'
    FocusControl = EDTpaymentCode
  end
  object sLabel6: TsLabel [6]
    Left = 174
    Top = 13
    Width = 175
    Height = 13
    Caption = 'Controlled on Order Chec&klist:'
    FocusControl = FRAMEFindordChklstdocs.DBE
  end
  inherited PNLfooter: TsPanel
    Width = 663
    TabOrder = 10
    Visible = False
  end
  object EDTpaymentCode: TsDBEdit [8]
    Left = 64
    Top = 8
    Width = 73
    Height = 21
    AutoSize = False
    CharCase = ecUpperCase
    Color = clWhite
    DataField = 'paymentCode'
    DataSource = DStable
    TabOrder = 0
    OnExit = EDTpaymentCodeExit
    SkinData.SkinSection = 'EDIT'
    BoundLabel.Indent = 0
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -11
    BoundLabel.Font.Name = 'MS Sans Serif'
    BoundLabel.Font.Style = []
    BoundLabel.Layout = sclLeft
    BoundLabel.MaxWidth = 0
    BoundLabel.UseSkinColor = True
  end
  inline FRAMEstatusInfo1: TFRAMEstatusInfo [9]
    Left = 8
    Top = 168
    Width = 466
    Height = 42
    AutoScroll = False
    ParentBackground = False
    TabOrder = 9
    inherited GRPstatus: TsGroupBox
      Width = 466
      inherited DBTXTlastUpd: TsDBText
        ParentFont = True
      end
```
<!-- tabs:end -->

