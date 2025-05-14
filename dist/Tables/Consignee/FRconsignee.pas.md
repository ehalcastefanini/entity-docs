<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código fornecido implementa um formulário para gerenciar informações relacionadas a consignatários (consignees). Ele permite que os usuários insiram, editem e visualizem dados como país, estado, mercado, armazém, termos de entrega, política de entrega, sistema de medidas, entre outros. O objetivo principal é facilitar a manipulação e validação de dados relacionados a consignatários em um sistema.

* **Tecnologias Utilizadas:**
  - Delphi (VCL Framework) para desenvolvimento da interface gráfica e lógica de negócios.
  - Componentes personalizados como `TFRAMEFindEditSOA`, `TFRAMEstatusInfo`, e `TsDBEdit` para funcionalidades específicas.
  - Serviços SOAP para integração com APIs externas.

* **Forma do Componente:**
  - **Formulário:**
    - **Elementos do Formulário e seus Tipos:**
      - Campos de texto (`TsDBEdit`) para entrada de dados como nome, código, número legal, etc.
      - Combobox de imagem (`TcxDBImageComboBox`) para seleção de modos de fatura.
      - Checkboxes (`TsDBCheckBox`) para opções booleanas como "Lista de embalagem detalhada".
      - Labels (`TsLabel`) para descrever os campos.
    - **Ações do Formulário e seus Efeitos:**
      - Ações de busca (`TFRAMEFindEditSOA`) para selecionar valores relacionados a país, estado, mercado, etc.
      - Validações e inicializações de dados ao interagir com os campos.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Os usuários podem preencher informações do consignatário, como nome, país, estado, mercado, etc.
  - Ações de busca permitem selecionar valores de listas predefinidas.
  - Validações são realizadas antes de salvar os dados.

* **Componentes Principais:**
  - `TFRAMEFindEditSOA`: Componente para busca de valores relacionados.
  - `TFRAMEstatusInfo`: Exibe informações de status.
  - `TsDBEdit`: Campos de entrada de dados vinculados ao banco de dados.

* **Tradução para Pseudo-código:**
  - Evento `OnClick` de um botão de busca: `se botão clicado então abrir janela de busca`.
  - Evento `OnChange` de um campo: `se valor do campo alterado então validar campo`.
  - Evento `OnExit` de um campo: `se campo perder foco então executar validação`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  - Inicialização: O formulário é carregado e os componentes são configurados.
  - Interação do Usuário: O usuário preenche os campos ou utiliza as ações de busca.
  - Validação: Os dados são validados antes de serem salvos.

* **Dados Necessários:**
  - Nome, código, país, estado, mercado, armazém, termos de entrega, política de entrega, sistema de medidas, entre outros.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ações de busca só podem ser realizadas se o campo correspondente estiver habilitado.
  - O botão de salvar só deve ser habilitado se todos os campos obrigatórios forem preenchidos.

* **Filtros Disponíveis:**
  - País, estado, mercado, armazém, termos de entrega, política de entrega, sistema de medidas, entre outros.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Valor inválido" se o valor inserido não for válido.

* **Valores Padrão dos Campos:**
  - Não especificado no código.

* **Validação de Campos:**
  - Não especificado no código.

---

## 5. Funções Principais:

* **Descrição das Funções:**
  - `m_SetFindCountry`: Configura o componente de busca para o país.
  - `m_SetFindState`: Configura o componente de busca para o estado.
  - `m_InitializeData`: Inicializa os dados do formulário.
  - `m_AfterApplyChanges`: Executa ações após salvar as alterações.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `ConsigneeServiceUtils`.
  - Endpoint: Não especificado no código.
  - Dados Enviados: Não especificado no código.
  - Dados Recebidos: Não especificado no código.
  - Propósito: Gerenciar dados de consignatários.
  - Tratamento de Erros: Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explicitamente definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `kneFRCtrlEditSOA`, `kneFRFindEditSOA`: Componentes personalizados para edição e busca.
  - `SOAPHTTPClient`: Para integração com serviços SOAP.

* **Componentes Personalizados:**
  - `TFRAMEFindEditSOA`, `TFRAMEstatusInfo`.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**
  - Nome (tipo: string, obrigatório).
  - Código (tipo: string, obrigatório).
  - País (tipo: string, obrigatório).
  - Estado (tipo: string, obrigatório).
  - Mercado (tipo: string, obrigatório).
  - Armazém (tipo: string, obrigatório).
  - Termos de Entrega (tipo: string, obrigatório).
  - Política de Entrega (tipo: string, obrigatório).
  - Sistema de Medidas (tipo: string, obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Trechos de Código:** Não aplicável.
* **Capturas de Tela:** Código DFM fornecido foi convertido para HTML:

```html
<div style="width: 983px; height: 417px; font-family: Verdana;">
  <label style="position: absolute; left: 8px; top: 65px; color: #4D4D4D;">Country:</label>
  <label style="position: absolute; left: 8px; top: 91px; color: #4D4D4D;">State:</label>
  <label style="position: absolute; left: 453px; top: 91px; color: #4D4D4D;">Market:</label>
  <label style="position: absolute; left: 8px; top: 117px; color: #4D4D4D;">Warehouse:</label>
  <label style="position: absolute; left: 8px; top: 169px; color: #4D4D4D;">Delivery Terms:</label>
  <label style="position: absolute; left: 453px; top: 169px; color: #4D4D4D;">Del Policy:</label>
  <label style="position: absolute; left: 8px; top: 195px; color: #4D4D4D;">Syst. of Measure:</label>
</div>
```

---

## 11. Comentários Importantes no Código:

* Comentário: `//NAVOPTECH2022-2563 (cmosilva 26-01-2023)` indica uma modificação ou adição específica no campo `EDTsapCode`.

---

## 12. Conclusão:

O código implementa um formulário robusto para gerenciar dados de consignatários, com suporte a validações e integração com serviços externos. No entanto, faltam detalhes sobre validações específicas e endpoints de serviços.

---

## 13. Resumo Curto:

O código define um formulário para gerenciar consignatários, com suporte a busca, validação e integração com serviços SOAP. Ele é parte de um sistema maior para manipulação de dados logísticos.#### **FRconsignee.pas**

```
unit FRconsignee;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRCtrlEditSOA, InvokeRegistry, ImgList, ActnList, ExtCtrls,
  Rio, SOAPHTTPClient, DB, DBClient, StdCtrls, Buttons, Mask, DBCtrls,
  kneFRFindEditSOA, cxGraphics, cxControls, cxContainer, cxEdit,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, cxDBEdit,
  kneFRStatusInfo, sFrameAdapter, sBitBtn, sPanel, sDBEdit, sLabel, DMskin,
  sCheckBox, sDBCheckBox;

type
  TFRAMEconsignee = class(TFRAMEBaseCtrlEditSOA)
    FRAMEfindConsMarket: TFRAMEFindEditSOA;
    FRAMEfindDestination: TFRAMEFindEditSOA;
    FRAMEfindDelPolicy: TFRAMEFindEditSOA;
    ICBOinvoiceMode: TcxDBImageComboBox;
    FRAMEstatusInfo1: TFRAMEstatusInfo;
    FRAMEfindCountry: TFRAMEFindEditSOA;
    FRAMEfindState: TFRAMEFindEditSOA;
    FRAMEfindWarehouse: TFRAMEFindEditSOA;
    FRAMEfindDelTerm: TFRAMEFindEditSOA;
    FRAMEfindUnitSys: TFRAMEFindEditSOA;
    FRAMEfindOrdDest: TFRAMEFindEditSOA;
    FRAMEfindLanguage: TFRAMEFindEditSOA;
    LBLcountry: TsLabel;
    LBLstate: TsLabel;
    LBLmarket: TsLabel;
    LBLwarehouse: TsLabel;
    Label2: TsLabel;
    LBLdelPolicy: TsLabel;
    LBLsysMesure: TsLabel;
    LBLname: TsLabel;
    Label3: TsLabel;
    Label4: TsLabel;
    LBLinvoiceMode: TsLabel;
    Label5: TsLabel;
    LBLdestination: TsLabel;
    Label1: TsLabel;
    Label6: TsLabel;
    EDTname: TsDBEdit;
    EDTabbrName: TsDBEdit;
    EDTlegalNum: TsDBEdit;
    EDTconsCode: TsDBEdit;
    CHKdetailPackList: TsDBCheckBox;
    LBL6: TsLabel;
    FRAMEfindShipMethod: TFRAMEFindEditSOA;
    LBL5: TsLabel;
    FRAMEFindFacing: TFRAMEFindEditSOA;
    LBL7: TsLabel;
    FRAMEFindSubFace: TFRAMEFindEditSOA;
    LBLsapCode: TsLabel;
    EDTsapCode: TsDBEdit; //NAVOPTECH2022-2563 (cmosilva 26-01-2023)
  private
    { Private declarations }
    mv_KeyInitVal: string;
    procedure m_SetFindCountry;
    procedure m_SetFindState;
    procedure m_SetFindWarehouse;
    procedure m_SetFindLanguage;
    procedure m_SetFindConsMarket;
    procedure m_SetFindDestination;
    procedure m_SetFindDelTerms;
    procedure m_SetFindDelPolicy;
    procedure m_SetFindUnitSys;
    procedure m_SetFindOrdDest;

    procedure m_BeforeFindConsMarket(Value: TObject);
    procedure m_BeforeFindWarehouse(Value: TObject);
    procedure m_BeforeFindState(Value: TObject);

    procedure m_InitializeData(Sender: TDataSet);
    procedure m_AfterApplyChanges(Sender: TObject);
    procedure m_SetFindShipping;
    procedure m_SetFindSegFacing;
    procedure m_BeforeFindTopCd(Sender: TObject);
    procedure m_ExitFindTopCd(Sender: TObject);

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure ShowData; override;
  end;

var
  FRAMEconsignee: TFRAMEconsignee;

implementation

uses
  kneTypes, kneFGFindUtils, kneUtils,
  //--- Frames, Forms
  MaddressAndContact, FRfindCriteriaDestination,
  //--- ServiceUtils
  ConsigneeServiceUtils,
  CountryServiceUtils, ConsigneeMarketChkAccessServiceUtils, DeliveryTermServiceUtils,
  LanguageServiceUtils, StateByCountryServiceUtils,
  DelPolicyServiceUtils, UnitSysServiceUtils,
```

#### **FRconsignee.dfm**

```
inherited FRAMEconsignee: TFRAMEconsignee
  Width = 983
  Height = 417
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  object LBLcountry: TsLabel [0]
    Left = 8
    Top = 65
    Width = 51
    Height = 13
    Caption = 'Country:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLstate: TsLabel [1]
    Left = 8
    Top = 91
    Width = 35
    Height = 13
    Caption = 'State:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLmarket: TsLabel [2]
    Left = 453
    Top = 91
    Width = 44
    Height = 13
    Caption = 'Market:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLwarehouse: TsLabel [3]
    Left = 8
    Top = 117
    Width = 69
    Height = 13
    Caption = 'Warehouse:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object Label2: TsLabel [4]
    Left = 8
    Top = 169
    Width = 93
    Height = 13
    Caption = 'Delivery Terms:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLdelPolicy: TsLabel [5]
    Left = 453
    Top = 169
    Width = 61
    Height = 13
    Caption = 'Del Policy:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLsysMesure: TsLabel [6]
    Left = 8
    Top = 195
    Width = 101
    Height = 13
    Caption = 'Syst. of Measure:'
    ParentFont = False
    Font.Charset = ANSI_CHARSET
    Font.Color = 5059883
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
  end
  object LBLname: TsLabel [7]
    Left = 8
    Top = 39
    Width = 38
```
<!-- tabs:end -->

